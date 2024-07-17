/* Lower _BitInt(N) operations to scalar operations.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "fold-const.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "cfgloop.h"
#include "cfganal.h"
#include "target.h"
#include "tree-ssa-live.h"
#include "tree-ssa-coalesce.h"
#include "domwalk.h"
#include "memmodel.h"
#include "optabs.h"
#include "varasm.h"
#include "gimple-range.h"
#include "value-range.h"
#include "langhooks.h"
#include "gimplify-me.h"
#include "diagnostic-core.h"
#include "tree-eh.h"
#include "tree-pretty-print.h"
#include "alloc-pool.h"
#include "tree-into-ssa.h"
#include "tree-cfgcleanup.h"
#include "tree-switch-conversion.h"
#include "ubsan.h"
#include "stor-layout.h"
#include "gimple-lower-bitint.h"

/* Split BITINT_TYPE precisions in 4 categories.  Small _BitInt, where
   target hook says it is a single limb, middle _BitInt which per ABI
   does not, but there is some INTEGER_TYPE in which arithmetics can be
   performed (operations on such _BitInt are lowered to casts to that
   arithmetic type and cast back; e.g. on x86_64 limb is DImode, but
   target supports TImode, so _BitInt(65) to _BitInt(128) are middle
   ones), large _BitInt which should by straight line code and
   finally huge _BitInt which should be handled by loops over the limbs.  */

enum bitint_prec_kind {
  bitint_prec_small,
  bitint_prec_middle,
  bitint_prec_large,
  bitint_prec_huge
};

/* Caches to speed up bitint_precision_kind.  */

static int small_max_prec, mid_min_prec, large_min_prec, huge_min_prec;
static int limb_prec;

/* Categorize _BitInt(PREC) as small, middle, large or huge.  */

static bitint_prec_kind
bitint_precision_kind (int prec)
{
  if (prec <= small_max_prec)
    return bitint_prec_small;
  if (huge_min_prec && prec >= huge_min_prec)
    return bitint_prec_huge;
  if (large_min_prec && prec >= large_min_prec)
    return bitint_prec_large;
  if (mid_min_prec && prec >= mid_min_prec)
    return bitint_prec_middle;

  struct bitint_info info;
  bool ok = targetm.c.bitint_type_info (prec, &info);
  gcc_assert (ok);
  scalar_int_mode limb_mode = as_a <scalar_int_mode> (info.limb_mode);
  if (prec <= GET_MODE_PRECISION (limb_mode))
    {
      small_max_prec = prec;
      return bitint_prec_small;
    }
  if (!large_min_prec
      && GET_MODE_PRECISION (limb_mode) < MAX_FIXED_MODE_SIZE)
    large_min_prec = MAX_FIXED_MODE_SIZE + 1;
  if (!limb_prec)
    limb_prec = GET_MODE_PRECISION (limb_mode);
  if (!huge_min_prec)
    {
      if (4 * limb_prec >= MAX_FIXED_MODE_SIZE)
	huge_min_prec = 4 * limb_prec;
      else
	huge_min_prec = MAX_FIXED_MODE_SIZE + 1;
    }
  if (prec <= MAX_FIXED_MODE_SIZE)
    {
      if (!mid_min_prec || prec < mid_min_prec)
	mid_min_prec = prec;
      return bitint_prec_middle;
    }
  if (large_min_prec && prec <= large_min_prec)
    return bitint_prec_large;
  return bitint_prec_huge;
}

/* Same for a TYPE.  */

static bitint_prec_kind
bitint_precision_kind (tree type)
{
  return bitint_precision_kind (TYPE_PRECISION (type));
}

/* Return minimum precision needed to describe INTEGER_CST
   CST.  All bits above that precision up to precision of
   TREE_TYPE (CST) are cleared if EXT is set to 0, or set
   if EXT is set to -1.  */

static unsigned
bitint_min_cst_precision (tree cst, int &ext)
{
  ext = tree_int_cst_sgn (cst) < 0 ? -1 : 0;
  wide_int w = wi::to_wide (cst);
  unsigned min_prec = wi::min_precision (w, TYPE_SIGN (TREE_TYPE (cst)));
  /* For signed values, we don't need to count the sign bit,
     we'll use constant 0 or -1 for the upper bits.  */
  if (!TYPE_UNSIGNED (TREE_TYPE (cst)))
    --min_prec;
  else
    {
      /* For unsigned values, also try signed min_precision
	 in case the constant has lots of most significant bits set.  */
      unsigned min_prec2 = wi::min_precision (w, SIGNED) - 1;
      if (min_prec2 < min_prec)
	{
	  ext = -1;
	  return min_prec2;
	}
    }
  return min_prec;
}

namespace {

/* If OP is middle _BitInt, cast it to corresponding INTEGER_TYPE
   cached in TYPE and return it.  */

tree
maybe_cast_middle_bitint (gimple_stmt_iterator *gsi, tree op, tree &type)
{
  if (op == NULL_TREE
      || TREE_CODE (TREE_TYPE (op)) != BITINT_TYPE
      || bitint_precision_kind (TREE_TYPE (op)) != bitint_prec_middle)
    return op;

  int prec = TYPE_PRECISION (TREE_TYPE (op));
  int uns = TYPE_UNSIGNED (TREE_TYPE (op));
  if (type == NULL_TREE
      || TYPE_PRECISION (type) != prec
      || TYPE_UNSIGNED (type) != uns)
    type = build_nonstandard_integer_type (prec, uns);

  if (TREE_CODE (op) != SSA_NAME)
    {
      tree nop = fold_convert (type, op);
      if (is_gimple_val (nop))
	return nop;
    }

  tree nop = make_ssa_name (type);
  gimple *g = gimple_build_assign (nop, NOP_EXPR, op);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  return nop;
}

/* Return true if STMT can be handled in a loop from least to most
   significant limb together with its dependencies.  */

bool
mergeable_op (gimple *stmt)
{
  if (!is_gimple_assign (stmt))
    return false;
  switch (gimple_assign_rhs_code (stmt))
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case NEGATE_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_NOT_EXPR:
    case SSA_NAME:
    case INTEGER_CST:
    case BIT_FIELD_REF:
      return true;
    case LSHIFT_EXPR:
      {
	tree cnt = gimple_assign_rhs2 (stmt);
	if (tree_fits_uhwi_p (cnt)
	    && tree_to_uhwi (cnt) < (unsigned HOST_WIDE_INT) limb_prec)
	  return true;
      }
      break;
    CASE_CONVERT:
    case VIEW_CONVERT_EXPR:
      {
	tree lhs_type = TREE_TYPE (gimple_assign_lhs (stmt));
	tree rhs_type = TREE_TYPE (gimple_assign_rhs1 (stmt));
	if (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
	    && TREE_CODE (lhs_type) == BITINT_TYPE
	    && TREE_CODE (rhs_type) == BITINT_TYPE
	    && bitint_precision_kind (lhs_type) >= bitint_prec_large
	    && bitint_precision_kind (rhs_type) >= bitint_prec_large
	    && (CEIL (TYPE_PRECISION (lhs_type), limb_prec)
		== CEIL (TYPE_PRECISION (rhs_type), limb_prec)))
	  {
	    if (TYPE_PRECISION (rhs_type) >= TYPE_PRECISION (lhs_type))
	      return true;
	    if ((unsigned) TYPE_PRECISION (lhs_type) % (2 * limb_prec) != 0)
	      return true;
	    if (bitint_precision_kind (lhs_type) == bitint_prec_large)
	      return true;
	  }
	break;
      }
    default:
      break;
    }
  return false;
}

/* Return non-zero if stmt is .{ADD,SUB,MUL}_OVERFLOW call with
   _Complex large/huge _BitInt lhs which has at most two immediate uses,
   at most one use in REALPART_EXPR stmt in the same bb and exactly one
   IMAGPART_EXPR use in the same bb with a single use which casts it to
   non-BITINT_TYPE integral type.  If there is a REALPART_EXPR use,
   return 2.  Such cases (most common uses of those builtins) can be
   optimized by marking their lhs and lhs of IMAGPART_EXPR and maybe lhs
   of REALPART_EXPR as not needed to be backed up by a stack variable.
   For .UBSAN_CHECK_{ADD,SUB,MUL} return 3.  */

int
optimizable_arith_overflow (gimple *stmt)
{
  bool is_ubsan = false;
  if (!is_gimple_call (stmt) || !gimple_call_internal_p (stmt))
    return false;
  switch (gimple_call_internal_fn (stmt))
    {
    case IFN_ADD_OVERFLOW:
    case IFN_SUB_OVERFLOW:
    case IFN_MUL_OVERFLOW:
      break;
    case IFN_UBSAN_CHECK_ADD:
    case IFN_UBSAN_CHECK_SUB:
    case IFN_UBSAN_CHECK_MUL:
      is_ubsan = true;
      break;
    default:
      return 0;
    }
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return 0;
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
    return 0;
  tree type = is_ubsan ? TREE_TYPE (lhs) : TREE_TYPE (TREE_TYPE (lhs));
  if (TREE_CODE (type) != BITINT_TYPE
      || bitint_precision_kind (type) < bitint_prec_large)
    return 0;

  if (is_ubsan)
    {
      use_operand_p use_p;
      gimple *use_stmt;
      if (!single_imm_use (lhs, &use_p, &use_stmt)
	  || gimple_bb (use_stmt) != gimple_bb (stmt)
	  || !gimple_store_p (use_stmt)
	  || !is_gimple_assign (use_stmt)
	  || gimple_has_volatile_ops (use_stmt)
	  || stmt_ends_bb_p (use_stmt))
	return 0;
      return 3;
    }

  imm_use_iterator ui;
  use_operand_p use_p;
  int seen = 0;
  gimple *realpart = NULL, *cast = NULL;
  FOR_EACH_IMM_USE_FAST (use_p, ui, lhs)
    {
      gimple *g = USE_STMT (use_p);
      if (is_gimple_debug (g))
	continue;
      if (!is_gimple_assign (g) || gimple_bb (g) != gimple_bb (stmt))
	return 0;
      if (gimple_assign_rhs_code (g) == REALPART_EXPR)
	{
	  if ((seen & 1) != 0)
	    return 0;
	  seen |= 1;
	  realpart = g;
	}
      else if (gimple_assign_rhs_code (g) == IMAGPART_EXPR)
	{
	  if ((seen & 2) != 0)
	    return 0;
	  seen |= 2;

	  use_operand_p use2_p;
	  gimple *use_stmt;
	  tree lhs2 = gimple_assign_lhs (g);
	  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs2))
	    return 0;
	  if (!single_imm_use (lhs2, &use2_p, &use_stmt)
	      || gimple_bb (use_stmt) != gimple_bb (stmt)
	      || !gimple_assign_cast_p (use_stmt))
	    return 0;

	  lhs2 = gimple_assign_lhs (use_stmt);
	  if (!INTEGRAL_TYPE_P (TREE_TYPE (lhs2))
	      || TREE_CODE (TREE_TYPE (lhs2)) == BITINT_TYPE)
	    return 0;
	  cast = use_stmt;
	}
      else
	return 0;
    }
  if ((seen & 2) == 0)
    return 0;
  if (seen == 3)
    {
      /* Punt if the cast stmt appears before realpart stmt, because
	 if both appear, the lowering wants to emit all the code
	 at the location of realpart stmt.  */
      gimple_stmt_iterator gsi = gsi_for_stmt (realpart);
      unsigned int cnt = 0;
      do
	{
	  gsi_prev_nondebug (&gsi);
	  if (gsi_end_p (gsi) || gsi_stmt (gsi) == cast)
	    return 0;
	  if (gsi_stmt (gsi) == stmt)
	    return 2;
	  /* If realpart is too far from stmt, punt as well.
	     Usually it will appear right after it.  */
	  if (++cnt == 32)
	    return 0;
	}
      while (1);
    }
  return 1;
}

/* If STMT is some kind of comparison (GIMPLE_COND, comparison assignment)
   comparing large/huge _BitInt types, return the comparison code and if
   non-NULL fill in the comparison operands to *POP1 and *POP2.  */

tree_code
comparison_op (gimple *stmt, tree *pop1, tree *pop2)
{
  tree op1 = NULL_TREE, op2 = NULL_TREE;
  tree_code code = ERROR_MARK;
  if (gimple_code (stmt) == GIMPLE_COND)
    {
      code = gimple_cond_code (stmt);
      op1 = gimple_cond_lhs (stmt);
      op2 = gimple_cond_rhs (stmt);
    }
  else if (is_gimple_assign (stmt))
    {
      code = gimple_assign_rhs_code (stmt);
      op1 = gimple_assign_rhs1 (stmt);
      if (TREE_CODE_CLASS (code) == tcc_comparison
	  || TREE_CODE_CLASS (code) == tcc_binary)
	op2 = gimple_assign_rhs2 (stmt);
    }
  if (TREE_CODE_CLASS (code) != tcc_comparison)
    return ERROR_MARK;
  tree type = TREE_TYPE (op1);
  if (TREE_CODE (type) != BITINT_TYPE
      || bitint_precision_kind (type) < bitint_prec_large)
    return ERROR_MARK;
  if (pop1)
    {
      *pop1 = op1;
      *pop2 = op2;
    }
  return code;
}

/* Class used during large/huge _BitInt lowering containing all the
   state for the methods.  */

struct bitint_large_huge
{
  bitint_large_huge ()
    : m_names (NULL), m_loads (NULL), m_preserved (NULL),
      m_single_use_names (NULL), m_map (NULL), m_vars (NULL),
      m_limb_type (NULL_TREE), m_data (vNULL),
      m_returns_twice_calls (vNULL) {}

  ~bitint_large_huge ();

  void insert_before (gimple *);
  tree limb_access_type (tree, tree);
  tree limb_access (tree, tree, tree, bool);
  tree build_bit_field_ref (tree, tree, unsigned HOST_WIDE_INT,
			    unsigned HOST_WIDE_INT);
  void if_then (gimple *, profile_probability, edge &, edge &);
  void if_then_else (gimple *, profile_probability, edge &, edge &);
  void if_then_if_then_else (gimple *g, gimple *,
			     profile_probability, profile_probability,
			     edge &, edge &, edge &);
  tree handle_operand (tree, tree);
  tree prepare_data_in_out (tree, tree, tree *, tree = NULL_TREE);
  tree add_cast (tree, tree);
  tree handle_plus_minus (tree_code, tree, tree, tree);
  tree handle_lshift (tree, tree, tree);
  tree handle_cast (tree, tree, tree);
  tree handle_bit_field_ref (tree, tree);
  tree handle_load (gimple *, tree);
  tree handle_stmt (gimple *, tree);
  tree handle_operand_addr (tree, gimple *, int *, int *);
  tree create_loop (tree, tree *);
  tree lower_mergeable_stmt (gimple *, tree_code &, tree, tree);
  tree lower_comparison_stmt (gimple *, tree_code &, tree, tree);
  void lower_shift_stmt (tree, gimple *);
  void lower_muldiv_stmt (tree, gimple *);
  void lower_float_conv_stmt (tree, gimple *);
  tree arith_overflow_extract_bits (unsigned int, unsigned int, tree,
				    unsigned int, bool);
  void finish_arith_overflow (tree, tree, tree, tree, tree, tree, gimple *,
			      tree_code);
  void lower_addsub_overflow (tree, gimple *);
  void lower_mul_overflow (tree, gimple *);
  void lower_cplxpart_stmt (tree, gimple *);
  void lower_complexexpr_stmt (gimple *);
  void lower_bit_query (gimple *);
  void lower_call (tree, gimple *);
  void lower_asm (gimple *);
  void lower_stmt (gimple *);

  /* Bitmap of large/huge _BitInt SSA_NAMEs except those can be
     merged with their uses.  */
  bitmap m_names;
  /* Subset of those for lhs of load statements.  These will be
     cleared in m_names if the loads will be mergeable with all
     their uses.  */
  bitmap m_loads;
  /* Bitmap of large/huge _BitInt SSA_NAMEs that should survive
     to later passes (arguments or return values of calls).  */
  bitmap m_preserved;
  /* Subset of m_names which have a single use.  As the lowering
     can replace various original statements with their lowered
     form even before it is done iterating over all basic blocks,
     testing has_single_use for the purpose of emitting clobbers
     doesn't work properly.  */
  bitmap m_single_use_names;
  /* Used for coalescing/partitioning of large/huge _BitInt SSA_NAMEs
     set in m_names.  */
  var_map m_map;
  /* Mapping of the partitions to corresponding decls.  */
  tree *m_vars;
  /* Unsigned integer type with limb precision.  */
  tree m_limb_type;
  /* Its TYPE_SIZE_UNIT.  */
  unsigned HOST_WIDE_INT m_limb_size;
  /* Location of a gimple stmt which is being currently lowered.  */
  location_t m_loc;
  /* Current stmt iterator where code is being lowered currently.  */
  gimple_stmt_iterator m_gsi;
  /* Statement after which any clobbers should be added if non-NULL.  */
  gimple *m_after_stmt;
  /* Set when creating loops to the loop header bb and its preheader.  */
  basic_block m_bb, m_preheader_bb;
  /* Stmt iterator after which initialization statements should be emitted.  */
  gimple_stmt_iterator m_init_gsi;
  /* Decl into which a mergeable statement stores result.  */
  tree m_lhs;
  /* handle_operand/handle_stmt can be invoked in various ways.

     lower_mergeable_stmt for large _BitInt calls those with constant
     idx only, expanding to straight line code, for huge _BitInt
     emits a loop from least significant limb upwards, where each loop
     iteration handles 2 limbs, plus there can be up to one full limb
     and one partial limb processed after the loop, where handle_operand
     and/or handle_stmt are called with constant idx.  m_upwards_2limb
     is set for this case, false otherwise.  m_upwards is true if it
     is either large or huge _BitInt handled by lower_mergeable_stmt,
     i.e. indexes always increase.

     Another way is used by lower_comparison_stmt, which walks limbs
     from most significant to least significant, partial limb if any
     processed first with constant idx and then loop processing a single
     limb per iteration with non-constant idx.

     Another way is used in lower_shift_stmt, where for LSHIFT_EXPR
     destination limbs are processed from most significant to least
     significant or for RSHIFT_EXPR the other way around, in loops or
     straight line code, but idx usually is non-constant (so from
     handle_operand/handle_stmt POV random access).  The LSHIFT_EXPR
     handling there can access even partial limbs using non-constant
     idx (then m_var_msb should be true, for all the other cases
     including lower_mergeable_stmt/lower_comparison_stmt that is
     not the case and so m_var_msb should be false.

     m_first should be set the first time handle_operand/handle_stmt
     is called and clear when it is called for some other limb with
     the same argument.  If the lowering of an operand (e.g. INTEGER_CST)
     or statement (e.g. +/-/<< with < limb_prec constant) needs some
     state between the different calls, when m_first is true it should
     push some trees to m_data vector and also make sure m_data_cnt is
     incremented by how many trees were pushed, and when m_first is
     false, it can use the m_data[m_data_cnt] etc. data or update them,
     just needs to bump m_data_cnt by the same amount as when it was
     called with m_first set.  The toplevel calls to
     handle_operand/handle_stmt should set m_data_cnt to 0 and truncate
     m_data vector when setting m_first to true.

     m_cast_conditional and m_bitfld_load are used when handling a
     bit-field load inside of a widening cast.  handle_cast sometimes
     needs to do runtime comparisons and handle_operand only conditionally
     or even in two separate conditional blocks for one idx (once with
     constant index after comparing the runtime one for equality with the
     constant).  In these cases, m_cast_conditional is set to true and
     the bit-field load then communicates its m_data_cnt to handle_cast
     using m_bitfld_load.  */
  bool m_first;
  bool m_var_msb;
  unsigned m_upwards_2limb;
  bool m_upwards;
  bool m_cast_conditional;
  unsigned m_bitfld_load;
  vec<tree> m_data;
  unsigned int m_data_cnt;
  vec<gimple *> m_returns_twice_calls;
};

bitint_large_huge::~bitint_large_huge ()
{
  BITMAP_FREE (m_names);
  BITMAP_FREE (m_loads);
  BITMAP_FREE (m_preserved);
  BITMAP_FREE (m_single_use_names);
  if (m_map)
    delete_var_map (m_map);
  XDELETEVEC (m_vars);
  m_data.release ();
  m_returns_twice_calls.release ();
}

/* Insert gimple statement G before current location
   and set its gimple_location.  */

void
bitint_large_huge::insert_before (gimple *g)
{
  gimple_set_location (g, m_loc);
  gsi_insert_before (&m_gsi, g, GSI_SAME_STMT);
}

/* Return type for accessing limb IDX of BITINT_TYPE TYPE.
   This is normally m_limb_type, except for a partial most
   significant limb if any.  */

tree
bitint_large_huge::limb_access_type (tree type, tree idx)
{
  if (type == NULL_TREE)
    return m_limb_type;
  unsigned HOST_WIDE_INT i = tree_to_uhwi (idx);
  unsigned int prec = TYPE_PRECISION (type);
  gcc_assert (i * limb_prec < prec);
  if ((i + 1) * limb_prec <= prec)
    return m_limb_type;
  else
    return build_nonstandard_integer_type (prec % limb_prec,
					   TYPE_UNSIGNED (type));
}

/* Return a tree how to access limb IDX of VAR corresponding to BITINT_TYPE
   TYPE.  If WRITE_P is true, it will be a store, otherwise a read.  */

tree
bitint_large_huge::limb_access (tree type, tree var, tree idx, bool write_p)
{
  tree atype = (tree_fits_uhwi_p (idx)
		? limb_access_type (type, idx) : m_limb_type);
  tree ltype = m_limb_type;
  addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (var));
  if (as != TYPE_ADDR_SPACE (ltype))
    ltype = build_qualified_type (ltype, TYPE_QUALS (ltype)
					 | ENCODE_QUAL_ADDR_SPACE (as));
  tree ret;
  if (DECL_P (var) && tree_fits_uhwi_p (idx))
    {
      tree ptype = build_pointer_type (strip_array_types (TREE_TYPE (var)));
      unsigned HOST_WIDE_INT off = tree_to_uhwi (idx) * m_limb_size;
      ret = build2 (MEM_REF, ltype,
		    build_fold_addr_expr (var),
		    build_int_cst (ptype, off));
      TREE_THIS_VOLATILE (ret) = TREE_THIS_VOLATILE (var);
      TREE_SIDE_EFFECTS (ret) = TREE_SIDE_EFFECTS (var);
    }
  else if (TREE_CODE (var) == MEM_REF && tree_fits_uhwi_p (idx))
    {
      ret
	= build2 (MEM_REF, ltype, unshare_expr (TREE_OPERAND (var, 0)),
		  size_binop (PLUS_EXPR, TREE_OPERAND (var, 1),
			      build_int_cst (TREE_TYPE (TREE_OPERAND (var, 1)),
					     tree_to_uhwi (idx)
					     * m_limb_size)));
      TREE_THIS_VOLATILE (ret) = TREE_THIS_VOLATILE (var);
      TREE_SIDE_EFFECTS (ret) = TREE_SIDE_EFFECTS (var);
      TREE_THIS_NOTRAP (ret) = TREE_THIS_NOTRAP (var);
    }
  else
    {
      var = unshare_expr (var);
      if (TREE_CODE (TREE_TYPE (var)) != ARRAY_TYPE
	  || !useless_type_conversion_p (m_limb_type,
					 TREE_TYPE (TREE_TYPE (var))))
	{
	  unsigned HOST_WIDE_INT nelts
	    = CEIL (tree_to_uhwi (TYPE_SIZE (TREE_TYPE (var))), limb_prec);
	  tree atype = build_array_type_nelts (ltype, nelts);
	  var = build1 (VIEW_CONVERT_EXPR, atype, var);
	}
      ret = build4 (ARRAY_REF, ltype, var, idx, NULL_TREE, NULL_TREE);
    }
  if (!write_p && !useless_type_conversion_p (atype, m_limb_type))
    {
      gimple *g = gimple_build_assign (make_ssa_name (m_limb_type), ret);
      insert_before (g);
      ret = gimple_assign_lhs (g);
      ret = build1 (NOP_EXPR, atype, ret);
    }
  return ret;
}

/* Build a BIT_FIELD_REF to access BITSIZE bits with FTYPE type at
   offset BITPOS inside of OBJ.  */

tree
bitint_large_huge::build_bit_field_ref (tree ftype, tree obj,
					unsigned HOST_WIDE_INT bitsize,
					unsigned HOST_WIDE_INT bitpos)
{
  if (INTEGRAL_TYPE_P (TREE_TYPE (obj))
      && !type_has_mode_precision_p (TREE_TYPE (obj)))
    {
      unsigned HOST_WIDE_INT nelts
	= CEIL (tree_to_uhwi (TYPE_SIZE (TREE_TYPE (obj))), limb_prec);
      tree ltype = m_limb_type;
      addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (obj));
      if (as != TYPE_ADDR_SPACE (ltype))
	ltype = build_qualified_type (ltype, TYPE_QUALS (ltype)
				      | ENCODE_QUAL_ADDR_SPACE (as));
      tree atype = build_array_type_nelts (ltype, nelts);
      obj = build1 (VIEW_CONVERT_EXPR, atype, obj);
    }
  return build3 (BIT_FIELD_REF, ftype, obj, bitsize_int (bitsize),
		 bitsize_int (bitpos));
}

/* Emit a half diamond,
   if (COND)
     |\
     | \
     |  \
     | new_bb1
     |  /
     | /
     |/
   or if (COND) new_bb1;
   PROB is the probability that the condition is true.
   Updates m_gsi to start of new_bb1.
   Sets EDGE_TRUE to edge from new_bb1 to successor and
   EDGE_FALSE to the EDGE_FALSE_VALUE edge from if (COND) bb.  */

void
bitint_large_huge::if_then (gimple *cond, profile_probability prob,
			    edge &edge_true, edge &edge_false)
{
  insert_before (cond);
  edge e1 = split_block (gsi_bb (m_gsi), cond);
  edge e2 = split_block (e1->dest, (gimple *) NULL);
  edge e3 = make_edge (e1->src, e2->dest, EDGE_FALSE_VALUE);
  e1->flags = EDGE_TRUE_VALUE;
  e1->probability = prob;
  e3->probability = prob.invert ();
  set_immediate_dominator (CDI_DOMINATORS, e2->dest, e1->src);
  edge_true = e2;
  edge_false = e3;
  m_gsi = gsi_after_labels (e1->dest);
}

/* Emit a full diamond,
       if (COND)
	 /\
	/  \
       /    \
   new_bb1 new_bb2
       \    /
	\  /
	 \/
   or if (COND) new_bb2; else new_bb1;
   PROB is the probability that the condition is true.
   Updates m_gsi to start of new_bb2.
   Sets EDGE_TRUE to edge from new_bb1 to successor and
   EDGE_FALSE to the EDGE_FALSE_VALUE edge from if (COND) bb.  */

void
bitint_large_huge::if_then_else (gimple *cond, profile_probability prob,
				 edge &edge_true, edge &edge_false)
{
  insert_before (cond);
  edge e1 = split_block (gsi_bb (m_gsi), cond);
  edge e2 = split_block (e1->dest, (gimple *) NULL);
  basic_block bb = create_empty_bb (e1->dest);
  add_bb_to_loop (bb, e1->dest->loop_father);
  edge e3 = make_edge (e1->src, bb, EDGE_TRUE_VALUE);
  e1->flags = EDGE_FALSE_VALUE;
  e3->probability = prob;
  e1->probability = prob.invert ();
  bb->count = e1->src->count.apply_probability (prob);
  set_immediate_dominator (CDI_DOMINATORS, bb, e1->src);
  set_immediate_dominator (CDI_DOMINATORS, e2->dest, e1->src);
  edge_true = make_single_succ_edge (bb, e2->dest, EDGE_FALLTHRU);
  edge_false = e2;
  m_gsi = gsi_after_labels (bb);
}

/* Emit a half diamond with full diamond in it
   if (COND1)
     |\
     | \
     |  \
     | if (COND2)
     |    /  \
     |   /    \
     |new_bb1 new_bb2
     |   |    /
     \   |   /
      \  |  /
       \ | /
	\|/
   or if (COND1) { if (COND2) new_bb2; else new_bb1; }
   PROB1 is the probability that the condition 1 is true.
   PROB2 is the probability that the condition 2 is true.
   Updates m_gsi to start of new_bb1.
   Sets EDGE_TRUE_TRUE to edge from new_bb2 to successor,
   EDGE_TRUE_FALSE to edge from new_bb1 to successor and
   EDGE_FALSE to the EDGE_FALSE_VALUE edge from if (COND1) bb.
   If COND2 is NULL, this is equivalent to
   if_then (COND1, PROB1, EDGE_TRUE_FALSE, EDGE_FALSE);
   EDGE_TRUE_TRUE = NULL;  */

void
bitint_large_huge::if_then_if_then_else (gimple *cond1, gimple *cond2,
					 profile_probability prob1,
					 profile_probability prob2,
					 edge &edge_true_true,
					 edge &edge_true_false,
					 edge &edge_false)
{
  edge e2, e3, e4 = NULL;
  if_then (cond1, prob1, e2, e3);
  if (cond2 == NULL)
    {
      edge_true_true = NULL;
      edge_true_false = e2;
      edge_false = e3;
      return;
    }
  insert_before (cond2);
  e2 = split_block (gsi_bb (m_gsi), cond2);
  basic_block bb = create_empty_bb (e2->dest);
  add_bb_to_loop (bb, e2->dest->loop_father);
  e4 = make_edge (e2->src, bb, EDGE_TRUE_VALUE);
  set_immediate_dominator (CDI_DOMINATORS, bb, e2->src);
  e4->probability = prob2;
  e2->flags = EDGE_FALSE_VALUE;
  e2->probability = prob2.invert ();
  bb->count = e2->src->count.apply_probability (prob2);
  e4 = make_single_succ_edge (bb, e3->dest, EDGE_FALLTHRU);
  e2 = find_edge (e2->dest, e3->dest);
  edge_true_true = e4;
  edge_true_false = e2;
  edge_false = e3;
  m_gsi = gsi_after_labels (e2->src);
}

/* Emit code to access limb IDX from OP.  */

tree
bitint_large_huge::handle_operand (tree op, tree idx)
{
  switch (TREE_CODE (op))
    {
    case SSA_NAME:
      if (m_names == NULL
	  || !bitmap_bit_p (m_names, SSA_NAME_VERSION (op)))
	{
	  if (SSA_NAME_IS_DEFAULT_DEF (op))
	    {
	      if (m_first)
		{
		  tree v = create_tmp_reg (m_limb_type);
		  if (SSA_NAME_VAR (op) && VAR_P (SSA_NAME_VAR (op)))
		    {
		      DECL_NAME (v) = DECL_NAME (SSA_NAME_VAR (op));
		      DECL_SOURCE_LOCATION (v)
			= DECL_SOURCE_LOCATION (SSA_NAME_VAR (op));
		    }
		  v = get_or_create_ssa_default_def (cfun, v);
		  m_data.safe_push (v);
		}
	      tree ret = m_data[m_data_cnt];
	      m_data_cnt++;
	      if (tree_fits_uhwi_p (idx))
		{
		  tree type = limb_access_type (TREE_TYPE (op), idx);
		  ret = add_cast (type, ret);
		}
	      return ret;
	    }
	  location_t loc_save = m_loc;
	  m_loc = gimple_location (SSA_NAME_DEF_STMT (op));
	  tree ret = handle_stmt (SSA_NAME_DEF_STMT (op), idx);
	  m_loc = loc_save;
	  return ret;
	}
      int p;
      gimple *g;
      tree t;
      p = var_to_partition (m_map, op);
      gcc_assert (m_vars[p] != NULL_TREE);
      t = limb_access (TREE_TYPE (op), m_vars[p], idx, false);
      g = gimple_build_assign (make_ssa_name (TREE_TYPE (t)), t);
      insert_before (g);
      t = gimple_assign_lhs (g);
      if (m_first
	  && m_single_use_names
	  && m_vars[p] != m_lhs
	  && m_after_stmt
	  && bitmap_bit_p (m_single_use_names, SSA_NAME_VERSION (op)))
	{
	  tree clobber = build_clobber (TREE_TYPE (m_vars[p]),
					CLOBBER_STORAGE_END);
	  g = gimple_build_assign (m_vars[p], clobber);
	  gimple_stmt_iterator gsi = gsi_for_stmt (m_after_stmt);
	  gsi_insert_after (&gsi, g, GSI_SAME_STMT);
	}
      return t;
    case INTEGER_CST:
      if (tree_fits_uhwi_p (idx))
	{
	  tree c, type = limb_access_type (TREE_TYPE (op), idx);
	  unsigned HOST_WIDE_INT i = tree_to_uhwi (idx);
	  if (m_first)
	    {
	      m_data.safe_push (NULL_TREE);
	      m_data.safe_push (NULL_TREE);
	    }
	  if (limb_prec != HOST_BITS_PER_WIDE_INT)
	    {
	      wide_int w = wi::rshift (wi::to_wide (op), i * limb_prec,
				       TYPE_SIGN (TREE_TYPE (op)));
	      c = wide_int_to_tree (type,
				    wide_int::from (w, TYPE_PRECISION (type),
						    UNSIGNED));
	    }
	  else if (i >= TREE_INT_CST_EXT_NUNITS (op))
	    c = build_int_cst (type,
			       tree_int_cst_sgn (op) < 0 ? -1 : 0);
	  else
	    c = build_int_cst (type, TREE_INT_CST_ELT (op, i));
	  m_data_cnt += 2;
	  return c;
	}
      if (m_first
	  || (m_data[m_data_cnt] == NULL_TREE
	      && m_data[m_data_cnt + 1] == NULL_TREE))
	{
	  unsigned int prec = TYPE_PRECISION (TREE_TYPE (op));
	  unsigned int rem = prec % ((m_upwards_2limb ? 2 : 1) * limb_prec);
	  int ext;
	  unsigned min_prec = bitint_min_cst_precision (op, ext);
	  if (m_first)
	    {
	      m_data.safe_push (NULL_TREE);
	      m_data.safe_push (NULL_TREE);
	    }
	  if (integer_zerop (op))
	    {
	      tree c = build_zero_cst (m_limb_type);
	      m_data[m_data_cnt] = c;
	      m_data[m_data_cnt + 1] = c;
	    }
	  else if (integer_all_onesp (op))
	    {
	      tree c = build_all_ones_cst (m_limb_type);
	      m_data[m_data_cnt] = c;
	      m_data[m_data_cnt + 1] = c;
	    }
	  else if (m_upwards_2limb && min_prec <= (unsigned) limb_prec)
	    {
	      /* Single limb constant.  Use a phi with that limb from
		 the preheader edge and 0 or -1 constant from the other edge
		 and for the second limb in the loop.  */
	      tree out;
	      gcc_assert (m_first);
	      m_data.pop ();
	      m_data.pop ();
	      prepare_data_in_out (fold_convert (m_limb_type, op), idx, &out,
				   build_int_cst (m_limb_type, ext));
	    }
	  else if (min_prec > prec - rem - 2 * limb_prec)
	    {
	      /* Constant which has enough significant bits that it isn't
		 worth trying to save .rodata space by extending from smaller
		 number.  */
	      tree type;
	      if (m_var_msb)
		type = TREE_TYPE (op);
	      else
		/* If we have a guarantee the most significant partial limb
		   (if any) will be only accessed through handle_operand
		   with INTEGER_CST idx, we don't need to include the partial
		   limb in .rodata.  */
		type = build_bitint_type (prec - rem, 1);
	      tree c = tree_output_constant_def (fold_convert (type, op));
	      m_data[m_data_cnt] = c;
	      m_data[m_data_cnt + 1] = NULL_TREE;
	    }
	  else if (m_upwards_2limb)
	    {
	      /* Constant with smaller number of bits.  Trade conditional
		 code for .rodata space by extending from smaller number.  */
	      min_prec = CEIL (min_prec, 2 * limb_prec) * (2 * limb_prec);
	      tree type = build_bitint_type (min_prec, 1);
	      tree c = tree_output_constant_def (fold_convert (type, op));
	      tree idx2 = make_ssa_name (sizetype);
	      g = gimple_build_assign (idx2, PLUS_EXPR, idx, size_one_node);
	      insert_before (g);
	      g = gimple_build_cond (LT_EXPR, idx,
				     size_int (min_prec / limb_prec),
				     NULL_TREE, NULL_TREE);
	      edge edge_true, edge_false;
	      if_then (g, (min_prec >= (prec - rem) / 2
			   ? profile_probability::likely ()
			   : profile_probability::unlikely ()),
		       edge_true, edge_false);
	      tree c1 = limb_access (TREE_TYPE (op), c, idx, false);
	      g = gimple_build_assign (make_ssa_name (TREE_TYPE (c1)), c1);
	      insert_before (g);
	      c1 = gimple_assign_lhs (g);
	      tree c2 = limb_access (TREE_TYPE (op), c, idx2, false);
	      g = gimple_build_assign (make_ssa_name (TREE_TYPE (c2)), c2);
	      insert_before (g);
	      c2 = gimple_assign_lhs (g);
	      tree c3 = build_int_cst (m_limb_type, ext);
	      m_gsi = gsi_after_labels (edge_true->dest);
	      m_data[m_data_cnt] = make_ssa_name (m_limb_type);
	      m_data[m_data_cnt + 1] = make_ssa_name (m_limb_type);
	      gphi *phi = create_phi_node (m_data[m_data_cnt],
					   edge_true->dest);
	      add_phi_arg (phi, c1, edge_true, UNKNOWN_LOCATION);
	      add_phi_arg (phi, c3, edge_false, UNKNOWN_LOCATION);
	      phi = create_phi_node (m_data[m_data_cnt + 1], edge_true->dest);
	      add_phi_arg (phi, c2, edge_true, UNKNOWN_LOCATION);
	      add_phi_arg (phi, c3, edge_false, UNKNOWN_LOCATION);
	    }
	  else
	    {
	      /* Constant with smaller number of bits.  Trade conditional
		 code for .rodata space by extending from smaller number.
		 Version for loops with random access to the limbs or
		 downwards loops.  */
	      min_prec = CEIL (min_prec, limb_prec) * limb_prec;
	      tree c;
	      if (min_prec <= (unsigned) limb_prec)
		c = fold_convert (m_limb_type, op);
	      else
		{
		  tree type = build_bitint_type (min_prec, 1);
		  c = tree_output_constant_def (fold_convert (type, op));
		}
	      m_data[m_data_cnt] = c;
	      m_data[m_data_cnt + 1] = integer_type_node;
	    }
	  t = m_data[m_data_cnt];
	  if (m_data[m_data_cnt + 1] == NULL_TREE)
	    {
	      t = limb_access (TREE_TYPE (op), t, idx, false);
	      g = gimple_build_assign (make_ssa_name (TREE_TYPE (t)), t);
	      insert_before (g);
	      t = gimple_assign_lhs (g);
	    }
	}
      else if (m_data[m_data_cnt + 1] == NULL_TREE)
	{
	  t = limb_access (TREE_TYPE (op), m_data[m_data_cnt], idx, false);
	  g = gimple_build_assign (make_ssa_name (TREE_TYPE (t)), t);
	  insert_before (g);
	  t = gimple_assign_lhs (g);
	}
      else
	t = m_data[m_data_cnt + 1];
      if (m_data[m_data_cnt + 1] == integer_type_node)
	{
	  unsigned int prec = TYPE_PRECISION (TREE_TYPE (op));
	  unsigned rem = prec % ((m_upwards_2limb ? 2 : 1) * limb_prec);
	  int ext = wi::neg_p (wi::to_wide (op)) ? -1 : 0;
	  tree c = m_data[m_data_cnt];
	  unsigned min_prec = TYPE_PRECISION (TREE_TYPE (c));
	  g = gimple_build_cond (LT_EXPR, idx,
				 size_int (min_prec / limb_prec),
				 NULL_TREE, NULL_TREE);
	  edge edge_true, edge_false;
	  if_then (g, (min_prec >= (prec - rem) / 2
		       ? profile_probability::likely ()
		       : profile_probability::unlikely ()),
		   edge_true, edge_false);
	  if (min_prec > (unsigned) limb_prec)
	    {
	      c = limb_access (TREE_TYPE (op), c, idx, false);
	      g = gimple_build_assign (make_ssa_name (TREE_TYPE (c)), c);
	      insert_before (g);
	      c = gimple_assign_lhs (g);
	    }
	  tree c2 = build_int_cst (m_limb_type, ext);
	  m_gsi = gsi_after_labels (edge_true->dest);
	  t = make_ssa_name (m_limb_type);
	  gphi *phi = create_phi_node (t, edge_true->dest);
	  add_phi_arg (phi, c, edge_true, UNKNOWN_LOCATION);
	  add_phi_arg (phi, c2, edge_false, UNKNOWN_LOCATION);
	}
      m_data_cnt += 2;
      return t;
    default:
      gcc_unreachable ();
    }
}

/* Helper method, add a PHI node with VAL from preheader edge if
   inside of a loop and m_first.  Keep state in a pair of m_data
   elements.  If VAL_OUT is non-NULL, use that as PHI argument from
   the latch edge, otherwise create a new SSA_NAME for it and let
   caller initialize it.  */

tree
bitint_large_huge::prepare_data_in_out (tree val, tree idx, tree *data_out,
					tree val_out)
{
  if (!m_first)
    {
      *data_out = tree_fits_uhwi_p (idx) ? NULL_TREE : m_data[m_data_cnt + 1];
      return m_data[m_data_cnt];
    }

  *data_out = NULL_TREE;
  if (tree_fits_uhwi_p (idx))
    {
      m_data.safe_push (val);
      m_data.safe_push (NULL_TREE);
      return val;
    }

  tree in = make_ssa_name (TREE_TYPE (val));
  gphi *phi = create_phi_node (in, m_bb);
  edge e1 = find_edge (m_preheader_bb, m_bb);
  edge e2 = EDGE_PRED (m_bb, 0);
  if (e1 == e2)
    e2 = EDGE_PRED (m_bb, 1);
  add_phi_arg (phi, val, e1, UNKNOWN_LOCATION);
  tree out = val_out ? val_out : make_ssa_name (TREE_TYPE (val));
  add_phi_arg (phi, out, e2, UNKNOWN_LOCATION);
  m_data.safe_push (in);
  m_data.safe_push (out);
  return in;
}

/* Return VAL cast to TYPE.  If VAL is INTEGER_CST, just
   convert it without emitting any code, otherwise emit
   the conversion statement before the current location.  */

tree
bitint_large_huge::add_cast (tree type, tree val)
{
  if (TREE_CODE (val) == INTEGER_CST)
    return fold_convert (type, val);

  tree lhs = make_ssa_name (type);
  gimple *g = gimple_build_assign (lhs, NOP_EXPR, val);
  insert_before (g);
  return lhs;
}

/* Helper of handle_stmt method, handle PLUS_EXPR or MINUS_EXPR.  */

tree
bitint_large_huge::handle_plus_minus (tree_code code, tree rhs1, tree rhs2,
				      tree idx)
{
  tree lhs, data_out, ctype;
  tree rhs1_type = TREE_TYPE (rhs1);
  gimple *g;
  tree data_in = prepare_data_in_out (build_zero_cst (m_limb_type), idx,
				      &data_out);

  if (optab_handler (code == PLUS_EXPR ? uaddc5_optab : usubc5_optab,
		     TYPE_MODE (m_limb_type)) != CODE_FOR_nothing)
    {
      ctype = build_complex_type (m_limb_type);
      if (!types_compatible_p (rhs1_type, m_limb_type))
	{
	  if (!TYPE_UNSIGNED (rhs1_type))
	    {
	      tree type = unsigned_type_for (rhs1_type);
	      rhs1 = add_cast (type, rhs1);
	      rhs2 = add_cast (type, rhs2);
	    }
	  rhs1 = add_cast (m_limb_type, rhs1);
	  rhs2 = add_cast (m_limb_type, rhs2);
	}
      lhs = make_ssa_name (ctype);
      g = gimple_build_call_internal (code == PLUS_EXPR
				      ? IFN_UADDC : IFN_USUBC,
				      3, rhs1, rhs2, data_in);
      gimple_call_set_lhs (g, lhs);
      insert_before (g);
      if (data_out == NULL_TREE)
	data_out = make_ssa_name (m_limb_type);
      g = gimple_build_assign (data_out, IMAGPART_EXPR,
			       build1 (IMAGPART_EXPR, m_limb_type, lhs));
      insert_before (g);
    }
  else if (types_compatible_p (rhs1_type, m_limb_type))
    {
      ctype = build_complex_type (m_limb_type);
      lhs = make_ssa_name (ctype);
      g = gimple_build_call_internal (code == PLUS_EXPR
				      ? IFN_ADD_OVERFLOW : IFN_SUB_OVERFLOW,
				      2, rhs1, rhs2);
      gimple_call_set_lhs (g, lhs);
      insert_before (g);
      if (data_out == NULL_TREE)
	data_out = make_ssa_name (m_limb_type);
      if (!integer_zerop (data_in))
	{
	  rhs1 = make_ssa_name (m_limb_type);
	  g = gimple_build_assign (rhs1, REALPART_EXPR,
				   build1 (REALPART_EXPR, m_limb_type, lhs));
	  insert_before (g);
	  rhs2 = make_ssa_name (m_limb_type);
	  g = gimple_build_assign (rhs2, IMAGPART_EXPR,
				   build1 (IMAGPART_EXPR, m_limb_type, lhs));
	  insert_before (g);
	  lhs = make_ssa_name (ctype);
	  g = gimple_build_call_internal (code == PLUS_EXPR
					  ? IFN_ADD_OVERFLOW
					  : IFN_SUB_OVERFLOW,
					  2, rhs1, data_in);
	  gimple_call_set_lhs (g, lhs);
	  insert_before (g);
	  data_in = make_ssa_name (m_limb_type);
	  g = gimple_build_assign (data_in, IMAGPART_EXPR,
				   build1 (IMAGPART_EXPR, m_limb_type, lhs));
	  insert_before (g);
	  g = gimple_build_assign (data_out, PLUS_EXPR, rhs2, data_in);
	  insert_before (g);
	}
      else
	{
	  g = gimple_build_assign (data_out, IMAGPART_EXPR,
				   build1 (IMAGPART_EXPR, m_limb_type, lhs));
	  insert_before (g);
	}
    }
  else
    {
      tree in = add_cast (rhs1_type, data_in);
      lhs = make_ssa_name (rhs1_type);
      g = gimple_build_assign (lhs, code, rhs1, rhs2);
      insert_before (g);
      rhs1 = make_ssa_name (rhs1_type);
      g = gimple_build_assign (rhs1, code, lhs, in);
      insert_before (g);
      m_data[m_data_cnt] = NULL_TREE;
      m_data_cnt += 2;
      return rhs1;
    }
  rhs1 = make_ssa_name (m_limb_type);
  g = gimple_build_assign (rhs1, REALPART_EXPR,
			   build1 (REALPART_EXPR, m_limb_type, lhs));
  insert_before (g);
  if (!types_compatible_p (rhs1_type, m_limb_type))
    rhs1 = add_cast (rhs1_type, rhs1);
  m_data[m_data_cnt] = data_out;
  m_data_cnt += 2;
  return rhs1;
}

/* Helper function for handle_stmt method, handle LSHIFT_EXPR by
   count in [0, limb_prec - 1] range.  */

tree
bitint_large_huge::handle_lshift (tree rhs1, tree rhs2, tree idx)
{
  unsigned HOST_WIDE_INT cnt = tree_to_uhwi (rhs2);
  gcc_checking_assert (cnt < (unsigned) limb_prec);
  if (cnt == 0)
    return rhs1;

  tree lhs, data_out, rhs1_type = TREE_TYPE (rhs1);
  gimple *g;
  tree data_in = prepare_data_in_out (build_zero_cst (m_limb_type), idx,
				      &data_out);

  if (!integer_zerop (data_in))
    {
      lhs = make_ssa_name (m_limb_type);
      g = gimple_build_assign (lhs, RSHIFT_EXPR, data_in,
			       build_int_cst (unsigned_type_node,
					      limb_prec - cnt));
      insert_before (g);
      if (!types_compatible_p (rhs1_type, m_limb_type))
	lhs = add_cast (rhs1_type, lhs);
      data_in = lhs;
    }
  if (types_compatible_p (rhs1_type, m_limb_type))
    {
      if (data_out == NULL_TREE)
	data_out = make_ssa_name (m_limb_type);
      g = gimple_build_assign (data_out, rhs1);
      insert_before (g);
    }
  if (cnt < (unsigned) TYPE_PRECISION (rhs1_type))
    {
      lhs = make_ssa_name (rhs1_type);
      g = gimple_build_assign (lhs, LSHIFT_EXPR, rhs1, rhs2);
      insert_before (g);
      if (!integer_zerop (data_in))
	{
	  rhs1 = lhs;
	  lhs = make_ssa_name (rhs1_type);
	  g = gimple_build_assign (lhs, BIT_IOR_EXPR, rhs1, data_in);
	  insert_before (g);
	}
    }
  else
    lhs = data_in;
  m_data[m_data_cnt] = data_out;
  m_data_cnt += 2;
  return lhs;
}

/* Helper function for handle_stmt method, handle an integral
   to integral conversion.  */

tree
bitint_large_huge::handle_cast (tree lhs_type, tree rhs1, tree idx)
{
  tree rhs_type = TREE_TYPE (rhs1);
  gimple *g;
  if ((TREE_CODE (rhs1) == SSA_NAME || TREE_CODE (rhs1) == INTEGER_CST)
      && TREE_CODE (lhs_type) == BITINT_TYPE
      && TREE_CODE (rhs_type) == BITINT_TYPE
      && bitint_precision_kind (lhs_type) >= bitint_prec_large
      && bitint_precision_kind (rhs_type) >= bitint_prec_large)
    {
      if (TYPE_PRECISION (rhs_type) >= TYPE_PRECISION (lhs_type)
	  /* If lhs has bigger precision than rhs, we can use
	     the simple case only if there is a guarantee that
	     the most significant limb is handled in straight
	     line code.  If m_var_msb (on left shifts) or
	     if m_upwards_2limb * limb_prec is equal to
	     lhs precision or if not m_upwards_2limb and lhs_type
	     has precision which is multiple of limb_prec that is
	     not the case.  */
	  || (!m_var_msb
	      && (CEIL (TYPE_PRECISION (lhs_type), limb_prec)
		  == CEIL (TYPE_PRECISION (rhs_type), limb_prec))
	      && ((!m_upwards_2limb
		   && (TYPE_PRECISION (lhs_type) % limb_prec != 0))
		  || (m_upwards_2limb
		      && (m_upwards_2limb * limb_prec
			  < TYPE_PRECISION (lhs_type))))))
	{
	  rhs1 = handle_operand (rhs1, idx);
	  if (tree_fits_uhwi_p (idx))
	    {
	      tree type = limb_access_type (lhs_type, idx);
	      if (!types_compatible_p (type, TREE_TYPE (rhs1)))
		rhs1 = add_cast (type, rhs1);
	    }
	  return rhs1;
	}
      tree t;
      /* Indexes lower than this don't need any special processing.  */
      unsigned low = ((unsigned) TYPE_PRECISION (rhs_type)
		      - !TYPE_UNSIGNED (rhs_type)) / limb_prec;
      /* Indexes >= than this always contain an extension.  */
      unsigned high = CEIL ((unsigned) TYPE_PRECISION (rhs_type), limb_prec);
      bool save_first = m_first;
      if (m_first)
	{
	  m_data.safe_push (NULL_TREE);
	  m_data.safe_push (NULL_TREE);
	  m_data.safe_push (NULL_TREE);
	  if (TYPE_UNSIGNED (rhs_type))
	    /* No need to keep state between iterations.  */
	    ;
	  else if (m_upwards && !m_upwards_2limb)
	    /* We need to keep state between iterations, but
	       not within any loop, everything is straight line
	       code with only increasing indexes.  */
	    ;
	  else if (!m_upwards_2limb)
	    {
	      unsigned save_data_cnt = m_data_cnt;
	      gimple_stmt_iterator save_gsi = m_gsi;
	      m_gsi = m_init_gsi;
	      if (gsi_end_p (m_gsi))
		m_gsi = gsi_after_labels (gsi_bb (m_gsi));
	      else
		gsi_next (&m_gsi);
	      m_data_cnt = save_data_cnt + 3;
	      t = handle_operand (rhs1, size_int (low));
	      m_first = false;
	      m_data[save_data_cnt + 2]
		= build_int_cst (NULL_TREE, m_data_cnt);
	      m_data_cnt = save_data_cnt;
	      t = add_cast (signed_type_for (m_limb_type), t);
	      tree lpm1 = build_int_cst (unsigned_type_node, limb_prec - 1);
	      tree n = make_ssa_name (TREE_TYPE (t));
	      g = gimple_build_assign (n, RSHIFT_EXPR, t, lpm1);
	      insert_before (g);
	      m_data[save_data_cnt + 1] = add_cast (m_limb_type, n);
	      m_init_gsi = m_gsi;
	      if (gsi_end_p (m_init_gsi))
		m_init_gsi = gsi_last_bb (gsi_bb (m_init_gsi));
	      else
		gsi_prev (&m_init_gsi);
	      m_gsi = save_gsi;
	    }
	  else if (m_upwards_2limb * limb_prec < TYPE_PRECISION (rhs_type))
	    /* We need to keep state between iterations, but
	       fortunately not within the loop, only afterwards.  */
	    ;
	  else
	    {
	      tree out;
	      m_data.truncate (m_data_cnt);
	      prepare_data_in_out (build_zero_cst (m_limb_type), idx, &out);
	      m_data.safe_push (NULL_TREE);
	    }
	}

      unsigned save_data_cnt = m_data_cnt;
      m_data_cnt += 3;
      if (!tree_fits_uhwi_p (idx))
	{
	  if (m_upwards_2limb
	      && low >= m_upwards_2limb - m_first)
	    {
	      rhs1 = handle_operand (rhs1, idx);
	      if (m_first)
		m_data[save_data_cnt + 2]
		  = build_int_cst (NULL_TREE, m_data_cnt);
	      m_first = save_first;
	      return rhs1;
	    }
	  bool single_comparison
	    = low == high || (m_upwards_2limb && (low & 1) == m_first);
	  tree idxc = idx;
	  if (!single_comparison
	      && m_upwards_2limb
	      && !m_first
	      && low + 1 == m_upwards_2limb)
	    /* In this case we know that idx <= low always,
	       so effectively we just needs a single comparison,
	       idx < low or idx == low, but we'd need to emit different
	       code for the 2 branches than single_comparison normally
	       emits.  So, instead of special-casing that, emit a
	       low <= low comparison which cfg cleanup will clean up
	       at the end of the pass.  */
	    idxc = size_int (low);
	  g = gimple_build_cond (single_comparison ? LT_EXPR : LE_EXPR,
				 idxc, size_int (low), NULL_TREE, NULL_TREE);
	  edge edge_true_true, edge_true_false, edge_false;
	  if_then_if_then_else (g, (single_comparison ? NULL
				    : gimple_build_cond (EQ_EXPR, idx,
							 size_int (low),
							 NULL_TREE,
							 NULL_TREE)),
				profile_probability::likely (),
				profile_probability::unlikely (),
				edge_true_true, edge_true_false, edge_false);
	  bool save_cast_conditional = m_cast_conditional;
	  m_cast_conditional = true;
	  m_bitfld_load = 0;
	  tree t1 = handle_operand (rhs1, idx), t2 = NULL_TREE;
	  if (m_first)
	    m_data[save_data_cnt + 2]
	      = build_int_cst (NULL_TREE, m_data_cnt);
	  tree ext = NULL_TREE;
	  tree bitfld = NULL_TREE;
	  if (!single_comparison)
	    {
	      m_gsi = gsi_after_labels (edge_true_true->src);
	      m_first = false;
	      m_data_cnt = save_data_cnt + 3;
	      if (m_bitfld_load)
		{
		  bitfld = m_data[m_bitfld_load];
		  m_data[m_bitfld_load] = m_data[m_bitfld_load + 2];
		  m_bitfld_load = 0;
		}
	      t2 = handle_operand (rhs1, size_int (low));
	      if (!useless_type_conversion_p (m_limb_type, TREE_TYPE (t2)))
		t2 = add_cast (m_limb_type, t2);
	      if (!TYPE_UNSIGNED (rhs_type) && m_upwards_2limb)
		{
		  ext = add_cast (signed_type_for (m_limb_type), t2);
		  tree lpm1 = build_int_cst (unsigned_type_node,
					     limb_prec - 1);
		  tree n = make_ssa_name (TREE_TYPE (ext));
		  g = gimple_build_assign (n, RSHIFT_EXPR, ext, lpm1);
		  insert_before (g);
		  ext = add_cast (m_limb_type, n);
		}
	    }
	  tree t3;
	  if (TYPE_UNSIGNED (rhs_type))
	    t3 = build_zero_cst (m_limb_type);
	  else if (m_upwards_2limb && (save_first || ext != NULL_TREE))
	    t3 = m_data[save_data_cnt];
	  else
	    t3 = m_data[save_data_cnt + 1];
	  m_gsi = gsi_after_labels (edge_true_false->dest);
	  t = make_ssa_name (m_limb_type);
	  gphi *phi = create_phi_node (t, edge_true_false->dest);
	  add_phi_arg (phi, t1, edge_true_false, UNKNOWN_LOCATION);
	  add_phi_arg (phi, t3, edge_false, UNKNOWN_LOCATION);
	  if (edge_true_true)
	    add_phi_arg (phi, t2, edge_true_true, UNKNOWN_LOCATION);
	  if (ext)
	    {
	      tree t4 = make_ssa_name (m_limb_type);
	      phi = create_phi_node (t4, edge_true_false->dest);
	      add_phi_arg (phi, build_zero_cst (m_limb_type), edge_true_false,
			   UNKNOWN_LOCATION);
	      add_phi_arg (phi, m_data[save_data_cnt], edge_false,
			   UNKNOWN_LOCATION);
	      add_phi_arg (phi, ext, edge_true_true, UNKNOWN_LOCATION);
	      if (!save_cast_conditional)
		{
		  g = gimple_build_assign (m_data[save_data_cnt + 1], t4);
		  insert_before (g);
		}
	      else
		for (basic_block bb = gsi_bb (m_gsi);;)
		  {
		    edge e1 = single_succ_edge (bb);
		    edge e2 = find_edge (e1->dest, m_bb), e3;
		    tree t5 = (e2 ? m_data[save_data_cnt + 1]
			       : make_ssa_name (m_limb_type));
		    phi = create_phi_node (t5, e1->dest);
		    edge_iterator ei;
		    FOR_EACH_EDGE (e3, ei, e1->dest->preds)
		      add_phi_arg (phi, (e3 == e1 ? t4
					 : build_zero_cst (m_limb_type)),
				   e3, UNKNOWN_LOCATION);
		    if (e2)
		      break;
		    t4 = t5;
		    bb = e1->dest;
		  }
	    }
	  if (m_bitfld_load)
	    {
	      tree t4;
	      if (!save_first && !save_cast_conditional)
		t4 = m_data[m_bitfld_load + 1];
	      else
		t4 = make_ssa_name (m_limb_type);
	      phi = create_phi_node (t4, edge_true_false->dest);
	      add_phi_arg (phi,
			   edge_true_true ? bitfld : m_data[m_bitfld_load],
			   edge_true_false, UNKNOWN_LOCATION);
	      add_phi_arg (phi, m_data[m_bitfld_load + 2],
			   edge_false, UNKNOWN_LOCATION);
	      if (edge_true_true)
		add_phi_arg (phi, m_data[m_bitfld_load], edge_true_true,
			     UNKNOWN_LOCATION);
	      if (save_cast_conditional)
		for (basic_block bb = gsi_bb (m_gsi);;)
		  {
		    edge e1 = single_succ_edge (bb);
		    edge e2 = find_edge (e1->dest, m_bb), e3;
		    tree t5 = ((e2 && !save_first) ? m_data[m_bitfld_load + 1]
			       : make_ssa_name (m_limb_type));
		    phi = create_phi_node (t5, e1->dest);
		    edge_iterator ei;
		    FOR_EACH_EDGE (e3, ei, e1->dest->preds)
		      add_phi_arg (phi, (e3 == e1 ? t4
					 : build_zero_cst (m_limb_type)),
				   e3, UNKNOWN_LOCATION);
		    t4 = t5;
		    if (e2)
		      break;
		    bb = e1->dest;
		  }
	      m_data[m_bitfld_load] = t4;
	      m_data[m_bitfld_load + 2] = t4;
	      m_bitfld_load = 0;
	    }
	  m_cast_conditional = save_cast_conditional;
	  m_first = save_first;
	  return t;
	}
      else
	{
	  if (tree_to_uhwi (idx) < low)
	    {
	      t = handle_operand (rhs1, idx);
	      if (m_first)
		m_data[save_data_cnt + 2]
		  = build_int_cst (NULL_TREE, m_data_cnt);
	    }
	  else if (tree_to_uhwi (idx) < high)
	    {
	      t = handle_operand (rhs1, size_int (low));
	      if (m_first)
		m_data[save_data_cnt + 2]
		  = build_int_cst (NULL_TREE, m_data_cnt);
	      if (!useless_type_conversion_p (m_limb_type, TREE_TYPE (t)))
		t = add_cast (m_limb_type, t);
	      tree ext = NULL_TREE;
	      if (!TYPE_UNSIGNED (rhs_type) && m_upwards)
		{
		  ext = add_cast (signed_type_for (m_limb_type), t);
		  tree lpm1 = build_int_cst (unsigned_type_node,
					     limb_prec - 1);
		  tree n = make_ssa_name (TREE_TYPE (ext));
		  g = gimple_build_assign (n, RSHIFT_EXPR, ext, lpm1);
		  insert_before (g);
		  ext = add_cast (m_limb_type, n);
		  m_data[save_data_cnt + 1] = ext;
		}
	    }
	  else
	    {
	      if (TYPE_UNSIGNED (rhs_type) && m_first)
		{
		  handle_operand (rhs1, size_zero_node);
		  m_data[save_data_cnt + 2]
		    = build_int_cst (NULL_TREE, m_data_cnt);
		}
	      else
		m_data_cnt = tree_to_uhwi (m_data[save_data_cnt + 2]);
	      if (TYPE_UNSIGNED (rhs_type))
		t = build_zero_cst (m_limb_type);
	      else if (m_bb && m_data[save_data_cnt])
		t = m_data[save_data_cnt];
	      else
		t = m_data[save_data_cnt + 1];
	    }
	  tree type = limb_access_type (lhs_type, idx);
	  if (!useless_type_conversion_p (type, m_limb_type))
	    t = add_cast (type, t);
	  m_first = save_first;
	  return t;
	}
    }
  else if (TREE_CODE (lhs_type) == BITINT_TYPE
	   && bitint_precision_kind (lhs_type) >= bitint_prec_large
	   && INTEGRAL_TYPE_P (rhs_type))
    {
      /* Add support for 3 or more limbs filled in from normal integral
	 type if this assert fails.  If no target chooses limb mode smaller
	 than half of largest supported normal integral type, this will not
	 be needed.  */
      gcc_assert (TYPE_PRECISION (rhs_type) <= 2 * limb_prec);
      tree r1 = NULL_TREE, r2 = NULL_TREE, rext = NULL_TREE;
      if (m_first)
	{
	  gimple_stmt_iterator save_gsi = m_gsi;
	  m_gsi = m_init_gsi;
	  if (gsi_end_p (m_gsi))
	    m_gsi = gsi_after_labels (gsi_bb (m_gsi));
	  else
	    gsi_next (&m_gsi);
	  if (TREE_CODE (rhs_type) == BITINT_TYPE
	      && bitint_precision_kind (rhs_type) == bitint_prec_middle)
	    {
	      tree type = NULL_TREE;
	      rhs1 = maybe_cast_middle_bitint (&m_gsi, rhs1, type);
	      rhs_type = TREE_TYPE (rhs1);
	    }
	  r1 = rhs1;
	  if (!useless_type_conversion_p (m_limb_type, TREE_TYPE (rhs1)))
	    r1 = add_cast (m_limb_type, rhs1);
	  if (TYPE_PRECISION (rhs_type) > limb_prec)
	    {
	      g = gimple_build_assign (make_ssa_name (rhs_type),
				       RSHIFT_EXPR, rhs1,
				       build_int_cst (unsigned_type_node,
						      limb_prec));
	      insert_before (g);
	      r2 = add_cast (m_limb_type, gimple_assign_lhs (g));
	    }
	  if (TYPE_UNSIGNED (rhs_type))
	    rext = build_zero_cst (m_limb_type);
	  else
	    {
	      rext = add_cast (signed_type_for (m_limb_type), r2 ? r2 : r1);
	      g = gimple_build_assign (make_ssa_name (TREE_TYPE (rext)),
				       RSHIFT_EXPR, rext,
				       build_int_cst (unsigned_type_node,
						      limb_prec - 1));
	      insert_before (g);
	      rext = add_cast (m_limb_type, gimple_assign_lhs (g));
	    }
	  m_init_gsi = m_gsi;
	  if (gsi_end_p (m_init_gsi))
	    m_init_gsi = gsi_last_bb (gsi_bb (m_init_gsi));
	  else
	    gsi_prev (&m_init_gsi);
	  m_gsi = save_gsi;
	}
      tree t;
      if (m_upwards_2limb)
	{
	  if (m_first)
	    {
	      tree out1, out2;
	      prepare_data_in_out (r1, idx, &out1, rext);
	      if (TYPE_PRECISION (rhs_type) > limb_prec)
		{
		  prepare_data_in_out (r2, idx, &out2, rext);
		  m_data.pop ();
		  t = m_data.pop ();
		  m_data[m_data_cnt + 1] = t;
		}
	      else
		m_data[m_data_cnt + 1] = rext;
	      m_data.safe_push (rext);
	      t = m_data[m_data_cnt];
	    }
	  else if (!tree_fits_uhwi_p (idx))
	    t = m_data[m_data_cnt + 1];
	  else
	    {
	      tree type = limb_access_type (lhs_type, idx);
	      t = m_data[m_data_cnt + 2];
	      if (!useless_type_conversion_p (type, m_limb_type))
		t = add_cast (type, t);
	    }
	  m_data_cnt += 3;
	  return t;
	}
      else if (m_first)
	{
	  m_data.safe_push (r1);
	  m_data.safe_push (r2);
	  m_data.safe_push (rext);
	}
      if (tree_fits_uhwi_p (idx))
	{
	  tree type = limb_access_type (lhs_type, idx);
	  if (integer_zerop (idx))
	    t = m_data[m_data_cnt];
	  else if (TYPE_PRECISION (rhs_type) > limb_prec
		   && integer_onep (idx))
	    t = m_data[m_data_cnt + 1];
	  else
	    t = m_data[m_data_cnt + 2];
	  if (!useless_type_conversion_p (type, m_limb_type))
	    t = add_cast (type, t);
	  m_data_cnt += 3;
	  return t;
	}
      g = gimple_build_cond (NE_EXPR, idx, size_zero_node,
			     NULL_TREE, NULL_TREE);
      edge e2, e3, e4 = NULL;
      if_then (g, profile_probability::likely (), e2, e3);
      if (m_data[m_data_cnt + 1])
	{
	  g = gimple_build_cond (EQ_EXPR, idx, size_one_node,
				 NULL_TREE, NULL_TREE);
	  insert_before (g);
	  edge e5 = split_block (gsi_bb (m_gsi), g);
	  e4 = make_edge (e5->src, e2->dest, EDGE_TRUE_VALUE);
	  e2 = find_edge (e5->dest, e2->dest);
	  e4->probability = profile_probability::unlikely ();
	  e5->flags = EDGE_FALSE_VALUE;
	  e5->probability = e4->probability.invert ();
	}
      m_gsi = gsi_after_labels (e2->dest);
      t = make_ssa_name (m_limb_type);
      gphi *phi = create_phi_node (t, e2->dest);
      add_phi_arg (phi, m_data[m_data_cnt + 2], e2, UNKNOWN_LOCATION);
      add_phi_arg (phi, m_data[m_data_cnt], e3, UNKNOWN_LOCATION);
      if (e4)
	add_phi_arg (phi, m_data[m_data_cnt + 1], e4, UNKNOWN_LOCATION);
      m_data_cnt += 3;
      return t;
    }
  return NULL_TREE;
}

/* Helper function for handle_stmt method, handle a BIT_FIELD_REF.  */

tree
bitint_large_huge::handle_bit_field_ref (tree op, tree idx)
{
  if (tree_fits_uhwi_p (idx))
    {
      if (m_first)
	m_data.safe_push (NULL);
      ++m_data_cnt;
      unsigned HOST_WIDE_INT sz = tree_to_uhwi (TYPE_SIZE (m_limb_type));
      tree bfr = build3 (BIT_FIELD_REF, m_limb_type,
			 TREE_OPERAND (op, 0),
			 TYPE_SIZE (m_limb_type),
			 size_binop (PLUS_EXPR, TREE_OPERAND (op, 2),
				     bitsize_int (tree_to_uhwi (idx) * sz)));
      tree r = make_ssa_name (m_limb_type);
      gimple *g = gimple_build_assign (r, bfr);
      insert_before (g);
      tree type = limb_access_type (TREE_TYPE (op), idx);
      if (!useless_type_conversion_p (type, m_limb_type))
	r = add_cast (type, r);
      return r;
    }
  tree var;
  if (m_first)
    {
      unsigned HOST_WIDE_INT sz = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (op)));
      machine_mode mode;
      tree type, bfr;
      if (bitwise_mode_for_size (sz).exists (&mode)
	  && known_eq (GET_MODE_BITSIZE (mode), sz))
	type = bitwise_type_for_mode (mode);
      else
	{
	  mode = VOIDmode;
	  type = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (op, 0)));
	}
      if (TYPE_ALIGN (type) < TYPE_ALIGN (TREE_TYPE (op)))
	type = build_aligned_type (type, TYPE_ALIGN (TREE_TYPE (op)));
      var = create_tmp_var (type);
      TREE_ADDRESSABLE (var) = 1;
      gimple *g;
      if (mode != VOIDmode)
	{
	  bfr = build3 (BIT_FIELD_REF, type, TREE_OPERAND (op, 0),
			TYPE_SIZE (type), TREE_OPERAND (op, 2));
	  g = gimple_build_assign (make_ssa_name (type),
				   BIT_FIELD_REF, bfr);
	  gimple_set_location (g, m_loc);
	  gsi_insert_after (&m_init_gsi, g, GSI_NEW_STMT);
	  bfr = gimple_assign_lhs (g);
	}
      else
	bfr = TREE_OPERAND (op, 0);
      g = gimple_build_assign (var, bfr);
      gimple_set_location (g, m_loc);
      gsi_insert_after (&m_init_gsi, g, GSI_NEW_STMT);
      if (mode == VOIDmode)
	{
	  unsigned HOST_WIDE_INT nelts
	    = CEIL (tree_to_uhwi (TYPE_SIZE (TREE_TYPE (op))), limb_prec);
	  tree atype = build_array_type_nelts (m_limb_type, nelts);
	  var = build2 (MEM_REF, atype, build_fold_addr_expr (var),
			build_int_cst (build_pointer_type (type),
				       tree_to_uhwi (TREE_OPERAND (op, 2))
				       / BITS_PER_UNIT));
	}
      m_data.safe_push (var);
    }
  else
    var = unshare_expr (m_data[m_data_cnt]);
  ++m_data_cnt;
  var = limb_access (TREE_TYPE (op), var, idx, false);
  tree r = make_ssa_name (m_limb_type);
  gimple *g = gimple_build_assign (r, var);
  insert_before (g);
  return r;
}

/* Add a new EH edge from SRC to EH_EDGE->dest, where EH_EDGE
   is an older EH edge, and except for virtual PHIs duplicate the
   PHI argument from the EH_EDGE to the new EH edge.  */

static void
add_eh_edge (basic_block src, edge eh_edge)
{
  edge e = make_edge (src, eh_edge->dest, EDGE_EH);
  e->probability = profile_probability::very_unlikely ();
  for (gphi_iterator gsi = gsi_start_phis (eh_edge->dest);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree lhs = gimple_phi_result (phi);
      if (virtual_operand_p (lhs))
	continue;
      const phi_arg_d *arg = gimple_phi_arg (phi, eh_edge->dest_idx);
      add_phi_arg (phi, arg->def, e, arg->locus);
    }
}

/* Helper function for handle_stmt method, handle a load from memory.  */

tree
bitint_large_huge::handle_load (gimple *stmt, tree idx)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs_type = TREE_TYPE (rhs1);
  bool eh = stmt_ends_bb_p (stmt);
  edge eh_edge = NULL;
  gimple *g;

  if (eh)
    {
      edge_iterator ei;
      basic_block bb = gimple_bb (stmt);

      FOR_EACH_EDGE (eh_edge, ei, bb->succs)
	if (eh_edge->flags & EDGE_EH)
	    break;
    }

  if (TREE_CODE (rhs1) == COMPONENT_REF
      && DECL_BIT_FIELD_TYPE (TREE_OPERAND (rhs1, 1)))
    {
      tree fld = TREE_OPERAND (rhs1, 1);
      /* For little-endian, we can allow as inputs bit-fields
	 which start at a limb boundary.  */
      gcc_assert (tree_fits_uhwi_p (DECL_FIELD_BIT_OFFSET (fld)));
      if (DECL_OFFSET_ALIGN (fld) >= TYPE_ALIGN (TREE_TYPE (rhs1))
	  && (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (fld)) % limb_prec) == 0)
	goto normal_load;
      /* Even if DECL_FIELD_BIT_OFFSET (fld) is a multiple of UNITS_PER_BIT,
	 handle it normally for now.  */
      if ((tree_to_uhwi (DECL_FIELD_BIT_OFFSET (fld)) % BITS_PER_UNIT) == 0)
	goto normal_load;
      tree repr = DECL_BIT_FIELD_REPRESENTATIVE (fld);
      poly_int64 bitoffset;
      poly_uint64 field_offset, repr_offset;
      bool var_field_off = false;
      if (poly_int_tree_p (DECL_FIELD_OFFSET (fld), &field_offset)
	  && poly_int_tree_p (DECL_FIELD_OFFSET (repr), &repr_offset))
	bitoffset = (field_offset - repr_offset) * BITS_PER_UNIT;
      else
	{
	  bitoffset = 0;
	  var_field_off = true;
	}
      bitoffset += (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (fld))
		    - tree_to_uhwi (DECL_FIELD_BIT_OFFSET (repr)));
      tree nrhs1 = build3 (COMPONENT_REF, TREE_TYPE (repr),
			   TREE_OPERAND (rhs1, 0), repr,
			   var_field_off ? TREE_OPERAND (rhs1, 2) : NULL_TREE);
      HOST_WIDE_INT bo = bitoffset.to_constant ();
      unsigned bo_idx = (unsigned HOST_WIDE_INT) bo / limb_prec;
      unsigned bo_bit = (unsigned HOST_WIDE_INT) bo % limb_prec;
      if (m_first)
	{
	  if (m_upwards)
	    {
	      gimple_stmt_iterator save_gsi = m_gsi;
	      m_gsi = m_init_gsi;
	      if (gsi_end_p (m_gsi))
		m_gsi = gsi_after_labels (gsi_bb (m_gsi));
	      else
		gsi_next (&m_gsi);
	      tree t = limb_access (NULL_TREE, nrhs1, size_int (bo_idx), true);
	      tree iv = make_ssa_name (m_limb_type);
	      g = gimple_build_assign (iv, t);
	      insert_before (g);
	      if (eh)
		{
		  maybe_duplicate_eh_stmt (g, stmt);
		  if (eh_edge)
		    {
		      edge e = split_block (gsi_bb (m_gsi), g);
		      add_eh_edge (e->src, eh_edge);
		      m_gsi = gsi_after_labels (e->dest);
		      if (gsi_bb (save_gsi) == e->src)
			{
			  if (gsi_end_p (save_gsi))
			    save_gsi = gsi_end_bb (e->dest);
			  else
			    save_gsi = gsi_for_stmt (gsi_stmt (save_gsi));
			}
		      if (m_preheader_bb == e->src)
			m_preheader_bb = e->dest;
		    }
		}
	      m_init_gsi = m_gsi;
	      if (gsi_end_p (m_init_gsi))
		m_init_gsi = gsi_last_bb (gsi_bb (m_init_gsi));
	      else
		gsi_prev (&m_init_gsi);
	      m_gsi = save_gsi;
	      tree out;
	      prepare_data_in_out (iv, idx, &out);
	      out = m_data[m_data_cnt];
	      m_data.safe_push (out);
	    }
	  else
	    {
	      m_data.safe_push (NULL_TREE);
	      m_data.safe_push (NULL_TREE);
	      m_data.safe_push (NULL_TREE);
	    }
	}

      tree nidx0 = NULL_TREE, nidx1;
      tree iv = m_data[m_data_cnt];
      if (m_cast_conditional && iv)
	{
	  gcc_assert (!m_bitfld_load);
	  m_bitfld_load = m_data_cnt;
	}
      if (tree_fits_uhwi_p (idx))
	{
	  unsigned prec = TYPE_PRECISION (rhs_type);
	  unsigned HOST_WIDE_INT i = tree_to_uhwi (idx);
	  gcc_assert (i * limb_prec < prec);
	  nidx1 = size_int (i + bo_idx + 1);
	  if ((i + 1) * limb_prec > prec)
	    {
	      prec %= limb_prec;
	      if (prec + bo_bit <= (unsigned) limb_prec)
		nidx1 = NULL_TREE;
	    }
	  if (!iv)
	    nidx0 = size_int (i + bo_idx);
	}
      else
	{
	  if (!iv)
	    {
	      if (bo_idx == 0)
		nidx0 = idx;
	      else
		{
		  nidx0 = make_ssa_name (sizetype);
		  g = gimple_build_assign (nidx0, PLUS_EXPR, idx,
					   size_int (bo_idx));
		  insert_before (g);
		}
	    }
	  nidx1 = make_ssa_name (sizetype);
	  g = gimple_build_assign (nidx1, PLUS_EXPR, idx,
				   size_int (bo_idx + 1));
	  insert_before (g);
	}

      tree iv2 = NULL_TREE;
      if (nidx0)
	{
	  tree t = limb_access (NULL_TREE, nrhs1, nidx0, true);
	  iv = make_ssa_name (m_limb_type);
	  g = gimple_build_assign (iv, t);
	  insert_before (g);
	  gcc_assert (!eh);
	}
      if (nidx1)
	{
	  bool conditional = m_var_msb && !tree_fits_uhwi_p (idx);
	  unsigned prec = TYPE_PRECISION (rhs_type);
	  if (conditional)
	    {
	      if ((prec % limb_prec) == 0
		  || ((prec % limb_prec) + bo_bit > (unsigned) limb_prec))
		conditional = false;
	    }
	  edge edge_true = NULL, edge_false = NULL;
	  if (conditional)
	    {
	      g = gimple_build_cond (NE_EXPR, idx,
				     size_int (prec / limb_prec),
				     NULL_TREE, NULL_TREE);
	      if_then (g, profile_probability::likely (),
		       edge_true, edge_false);
	    }
	  tree t = limb_access (NULL_TREE, nrhs1, nidx1, true);
	  if (m_upwards_2limb
	      && !m_first
	      && !m_bitfld_load
	      && !tree_fits_uhwi_p (idx))
	    iv2 = m_data[m_data_cnt + 1];
	  else
	    iv2 = make_ssa_name (m_limb_type);
	  g = gimple_build_assign (iv2, t);
	  insert_before (g);
	  if (eh)
	    {
	      maybe_duplicate_eh_stmt (g, stmt);
	      if (eh_edge)
		{
		  edge e = split_block (gsi_bb (m_gsi), g);
		  m_gsi = gsi_after_labels (e->dest);
		  add_eh_edge (e->src, eh_edge);
		}
	    }
	  if (conditional)
	    {
	      tree iv3 = make_ssa_name (m_limb_type);
	      if (eh)
		edge_true = find_edge (gsi_bb (m_gsi), edge_false->dest);
	      gphi *phi = create_phi_node (iv3, edge_true->dest);
	      add_phi_arg (phi, iv2, edge_true, UNKNOWN_LOCATION);
	      add_phi_arg (phi, build_zero_cst (m_limb_type),
			   edge_false, UNKNOWN_LOCATION);
	      m_gsi = gsi_after_labels (edge_true->dest);
	      iv2 = iv3;
	    }
	}
      g = gimple_build_assign (make_ssa_name (m_limb_type), RSHIFT_EXPR,
			       iv, build_int_cst (unsigned_type_node, bo_bit));
      insert_before (g);
      iv = gimple_assign_lhs (g);
      if (iv2)
	{
	  g = gimple_build_assign (make_ssa_name (m_limb_type), LSHIFT_EXPR,
				   iv2, build_int_cst (unsigned_type_node,
						       limb_prec - bo_bit));
	  insert_before (g);
	  g = gimple_build_assign (make_ssa_name (m_limb_type), BIT_IOR_EXPR,
				   gimple_assign_lhs (g), iv);
	  insert_before (g);
	  iv = gimple_assign_lhs (g);
	  if (m_data[m_data_cnt])
	    m_data[m_data_cnt] = iv2;
	}
      if (tree_fits_uhwi_p (idx))
	{
	  tree atype = limb_access_type (rhs_type, idx);
	  if (!useless_type_conversion_p (atype, TREE_TYPE (iv)))
	    iv = add_cast (atype, iv);
	}
      m_data_cnt += 3;
      return iv;
    }

normal_load:
  /* Use write_p = true for loads with EH edges to make
     sure limb_access doesn't add a cast as separate
     statement after it.  */
  rhs1 = limb_access (rhs_type, rhs1, idx, eh);
  tree ret = make_ssa_name (TREE_TYPE (rhs1));
  g = gimple_build_assign (ret, rhs1);
  insert_before (g);
  if (eh)
    {
      maybe_duplicate_eh_stmt (g, stmt);
      if (eh_edge)
	{
	  edge e = split_block (gsi_bb (m_gsi), g);
	  m_gsi = gsi_after_labels (e->dest);
	  add_eh_edge (e->src, eh_edge);
	}
      if (tree_fits_uhwi_p (idx))
	{
	  tree atype = limb_access_type (rhs_type, idx);
	  if (!useless_type_conversion_p (atype, TREE_TYPE (rhs1)))
	    ret = add_cast (atype, ret);
	}
    }
  return ret;
}

/* Return a limb IDX from a mergeable statement STMT.  */

tree
bitint_large_huge::handle_stmt (gimple *stmt, tree idx)
{
  tree lhs, rhs1, rhs2 = NULL_TREE;
  gimple *g;
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      if (gimple_assign_load_p (stmt))
	return handle_load (stmt, idx);
      switch (gimple_assign_rhs_code (stmt))
	{
	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	  rhs2 = handle_operand (gimple_assign_rhs2 (stmt), idx);
	  /* FALLTHRU */
	case BIT_NOT_EXPR:
	  rhs1 = handle_operand (gimple_assign_rhs1 (stmt), idx);
	  lhs = make_ssa_name (TREE_TYPE (rhs1));
	  g = gimple_build_assign (lhs, gimple_assign_rhs_code (stmt),
				   rhs1, rhs2);
	  insert_before (g);
	  return lhs;
	case PLUS_EXPR:
	case MINUS_EXPR:
	  rhs1 = handle_operand (gimple_assign_rhs1 (stmt), idx);
	  rhs2 = handle_operand (gimple_assign_rhs2 (stmt), idx);
	  return handle_plus_minus (gimple_assign_rhs_code (stmt),
				    rhs1, rhs2, idx);
	case NEGATE_EXPR:
	  rhs2 = handle_operand (gimple_assign_rhs1 (stmt), idx);
	  rhs1 = build_zero_cst (TREE_TYPE (rhs2));
	  return handle_plus_minus (MINUS_EXPR, rhs1, rhs2, idx);
	case LSHIFT_EXPR:
	  return handle_lshift (handle_operand (gimple_assign_rhs1 (stmt),
						idx),
				gimple_assign_rhs2 (stmt), idx);
	case SSA_NAME:
	case INTEGER_CST:
	  return handle_operand (gimple_assign_rhs1 (stmt), idx);
	CASE_CONVERT:
	  return handle_cast (TREE_TYPE (gimple_assign_lhs (stmt)),
			      gimple_assign_rhs1 (stmt), idx);
	case VIEW_CONVERT_EXPR:
	  return handle_cast (TREE_TYPE (gimple_assign_lhs (stmt)),
			      TREE_OPERAND (gimple_assign_rhs1 (stmt), 0),
			      idx);
	case BIT_FIELD_REF:
	  return handle_bit_field_ref (gimple_assign_rhs1 (stmt), idx);
	default:
	  break;
	}
      break;
    default:
      break;
    }
  gcc_unreachable ();
}

/* Return minimum precision of OP at STMT.
   Positive value is minimum precision above which all bits
   are zero, negative means all bits above negation of the
   value are copies of the sign bit.  */

static int
range_to_prec (tree op, gimple *stmt)
{
  int_range_max r;
  wide_int w;
  tree type = TREE_TYPE (op);
  unsigned int prec = TYPE_PRECISION (type);

  if (!optimize
      || !get_range_query (cfun)->range_of_expr (r, op, stmt)
      || r.undefined_p ())
    {
      if (TYPE_UNSIGNED (type))
	return prec;
      else
	return MIN ((int) -prec, -2);
    }

  if (!TYPE_UNSIGNED (TREE_TYPE (op)))
    {
      w = r.lower_bound ();
      if (wi::neg_p (w))
	{
	  int min_prec1 = wi::min_precision (w, SIGNED);
	  w = r.upper_bound ();
	  int min_prec2 = wi::min_precision (w, SIGNED);
	  int min_prec = MAX (min_prec1, min_prec2);
	  return MIN (-min_prec, -2);
	}
    }

  w = r.upper_bound ();
  int min_prec = wi::min_precision (w, UNSIGNED);
  return MAX (min_prec, 1);
}

/* Return address of the first limb of OP and write into *PREC
   its precision.  If positive, the operand is zero extended
   from that precision, if it is negative, the operand is sign-extended
   from -*PREC.  If PREC_STORED is NULL, it is the toplevel call,
   otherwise *PREC_STORED is prec from the innermost call without
   range optimizations.  */

tree
bitint_large_huge::handle_operand_addr (tree op, gimple *stmt,
					int *prec_stored, int *prec)
{
  wide_int w;
  location_t loc_save = m_loc;
  if ((TREE_CODE (TREE_TYPE (op)) != BITINT_TYPE
       || bitint_precision_kind (TREE_TYPE (op)) < bitint_prec_large)
      && TREE_CODE (op) != INTEGER_CST)
    {
    do_int:
      *prec = range_to_prec (op, stmt);
      bitint_prec_kind kind = bitint_prec_small;
      gcc_assert (INTEGRAL_TYPE_P (TREE_TYPE (op)));
      if (TREE_CODE (TREE_TYPE (op)) == BITINT_TYPE)
	kind = bitint_precision_kind (TREE_TYPE (op));
      if (kind == bitint_prec_middle)
	{
	  tree type = NULL_TREE;
	  op = maybe_cast_middle_bitint (&m_gsi, op, type);
	}
      tree op_type = TREE_TYPE (op);
      unsigned HOST_WIDE_INT nelts
	= CEIL (TYPE_PRECISION (op_type), limb_prec);
      /* Add support for 3 or more limbs filled in from normal
	 integral type if this assert fails.  If no target chooses
	 limb mode smaller than half of largest supported normal
	 integral type, this will not be needed.  */
      gcc_assert (nelts <= 2);
      if (prec_stored)
	*prec_stored = (TYPE_UNSIGNED (op_type)
			? TYPE_PRECISION (op_type)
			: -TYPE_PRECISION (op_type));
      if (*prec <= limb_prec && *prec >= -limb_prec)
	{
	  nelts = 1;
	  if (prec_stored)
	    {
	      if (TYPE_UNSIGNED (op_type))
		{
		  if (*prec_stored > limb_prec)
		    *prec_stored = limb_prec;
		}
	      else if (*prec_stored < -limb_prec)
		*prec_stored = -limb_prec;
	    }
	}
      tree atype = build_array_type_nelts (m_limb_type, nelts);
      tree var = create_tmp_var (atype);
      tree t1 = op;
      if (!useless_type_conversion_p (m_limb_type, op_type))
	t1 = add_cast (m_limb_type, t1);
      tree v = build4 (ARRAY_REF, m_limb_type, var, size_zero_node,
		       NULL_TREE, NULL_TREE);
      gimple *g = gimple_build_assign (v, t1);
      insert_before (g);
      if (nelts > 1)
	{
	  tree lp = build_int_cst (unsigned_type_node, limb_prec);
	  g = gimple_build_assign (make_ssa_name (op_type),
				   RSHIFT_EXPR, op, lp);
	  insert_before (g);
	  tree t2 = gimple_assign_lhs (g);
	  t2 = add_cast (m_limb_type, t2);
	  v = build4 (ARRAY_REF, m_limb_type, var, size_one_node,
		      NULL_TREE, NULL_TREE);
	  g = gimple_build_assign (v, t2);
	  insert_before (g);
	}
      tree ret = build_fold_addr_expr (var);
      if (!stmt_ends_bb_p (gsi_stmt (m_gsi)))
	{
	  tree clobber = build_clobber (atype, CLOBBER_STORAGE_END);
	  g = gimple_build_assign (var, clobber);
	  gsi_insert_after (&m_gsi, g, GSI_SAME_STMT);
	}
      m_loc = loc_save;
      return ret;
    }
  switch (TREE_CODE (op))
    {
    case SSA_NAME:
      if (m_names == NULL
	  || !bitmap_bit_p (m_names, SSA_NAME_VERSION (op)))
	{
	  gimple *g = SSA_NAME_DEF_STMT (op);
	  tree ret;
	  m_loc = gimple_location (g);
	  if (gimple_assign_load_p (g))
	    {
	      *prec = range_to_prec (op, NULL);
	      if (prec_stored)
		*prec_stored = (TYPE_UNSIGNED (TREE_TYPE (op))
				? TYPE_PRECISION (TREE_TYPE (op))
				: -TYPE_PRECISION (TREE_TYPE (op)));
	      ret = build_fold_addr_expr (gimple_assign_rhs1 (g));
	      ret = force_gimple_operand_gsi (&m_gsi, ret, true,
					      NULL_TREE, true, GSI_SAME_STMT);
	    }
	  else if (gimple_code (g) == GIMPLE_NOP)
	    {
	      *prec = TYPE_UNSIGNED (TREE_TYPE (op)) ? limb_prec : -limb_prec;
	      if (prec_stored)
		*prec_stored = *prec;
	      tree var = create_tmp_var (m_limb_type);
	      TREE_ADDRESSABLE (var) = 1;
	      ret = build_fold_addr_expr (var);
	      if (!stmt_ends_bb_p (gsi_stmt (m_gsi)))
		{
		  tree clobber = build_clobber (m_limb_type,
						CLOBBER_STORAGE_END);
		  g = gimple_build_assign (var, clobber);
		  gsi_insert_after (&m_gsi, g, GSI_SAME_STMT);
		}
	    }
	  else
	    {
	      gcc_assert (gimple_assign_cast_p (g));
	      tree rhs1 = gimple_assign_rhs1 (g);
	      bitint_prec_kind kind = bitint_prec_small;
	      if (TREE_CODE (rhs1) == VIEW_CONVERT_EXPR)
		rhs1 = TREE_OPERAND (rhs1, 0);
	      gcc_assert (INTEGRAL_TYPE_P (TREE_TYPE (rhs1)));
	      if (TREE_CODE (TREE_TYPE (rhs1)) == BITINT_TYPE)
		kind = bitint_precision_kind (TREE_TYPE (rhs1));
	      if (kind >= bitint_prec_large)
		{
		  tree lhs_type = TREE_TYPE (op);
		  tree rhs_type = TREE_TYPE (rhs1);
		  int prec_stored_val = 0;
		  ret = handle_operand_addr (rhs1, g, &prec_stored_val, prec);
		  if (TYPE_PRECISION (lhs_type) > TYPE_PRECISION (rhs_type))
		    {
		      if (TYPE_UNSIGNED (lhs_type)
			  && !TYPE_UNSIGNED (rhs_type))
			gcc_assert (*prec >= 0 || prec_stored == NULL);
		    }
		  else
		    {
		      if (*prec > 0 && *prec < TYPE_PRECISION (lhs_type))
			;
		      else if (TYPE_UNSIGNED (lhs_type))
			{
			  gcc_assert (*prec > 0
				      || prec_stored_val > 0
				      || (-prec_stored_val
					  >= TYPE_PRECISION (lhs_type)));
			  *prec = TYPE_PRECISION (lhs_type);
			}
		      else if (*prec < 0 && -*prec < TYPE_PRECISION (lhs_type))
			;
		      else
			*prec = -TYPE_PRECISION (lhs_type);
		    }
		}
	      else
		{
		  op = rhs1;
		  stmt = g;
		  goto do_int;
		}
	    }
	  m_loc = loc_save;
	  return ret;
	}
      else
	{
	  int p = var_to_partition (m_map, op);
	  gcc_assert (m_vars[p] != NULL_TREE);
	  *prec = range_to_prec (op, stmt);
	  if (prec_stored)
	    *prec_stored = (TYPE_UNSIGNED (TREE_TYPE (op))
			    ? TYPE_PRECISION (TREE_TYPE (op))
			    : -TYPE_PRECISION (TREE_TYPE (op)));
	  return build_fold_addr_expr (m_vars[p]);
	}
    case INTEGER_CST:
      unsigned int min_prec, mp;
      tree type;
      w = wi::to_wide (op);
      if (tree_int_cst_sgn (op) >= 0)
	{
	  min_prec = wi::min_precision (w, UNSIGNED);
	  *prec = MAX (min_prec, 1);
	}
      else
	{
	  min_prec = wi::min_precision (w, SIGNED);
	  *prec = MIN ((int) -min_prec, -2);
	}
      mp = CEIL (min_prec, limb_prec) * limb_prec;
      if (mp == 0)
	mp = 1;
      if (mp >= (unsigned) TYPE_PRECISION (TREE_TYPE (op))
	  && (TREE_CODE (TREE_TYPE (op)) == BITINT_TYPE
	      || TYPE_PRECISION (TREE_TYPE (op)) <= limb_prec))
	type = TREE_TYPE (op);
      else
	type = build_bitint_type (mp, 1);
      if (TREE_CODE (type) != BITINT_TYPE
	  || bitint_precision_kind (type) == bitint_prec_small)
	{
	  if (TYPE_PRECISION (type) <= limb_prec)
	    type = m_limb_type;
	  else
	    {
	      while (bitint_precision_kind (mp) == bitint_prec_small)
		mp += limb_prec;
	      /* This case is for targets which e.g. have 64-bit
		 limb but categorize up to 128-bits _BitInts as
		 small.  We could use type of m_limb_type[2] and
		 similar instead to save space.  */
	      type = build_bitint_type (mp, 1);
	    }
	}
      if (prec_stored)
	{
	  if (tree_int_cst_sgn (op) >= 0)
	    *prec_stored = MAX (TYPE_PRECISION (type), 1);
	  else
	    *prec_stored = MIN ((int) -TYPE_PRECISION (type), -2);
	}
      op = tree_output_constant_def (fold_convert (type, op));
      return build_fold_addr_expr (op);
    default:
      gcc_unreachable ();
    }
}

/* Helper function, create a loop before the current location,
   start with sizetype INIT value from the preheader edge.  Return
   a PHI result and set *IDX_NEXT to SSA_NAME it creates and uses
   from the latch edge.  */

tree
bitint_large_huge::create_loop (tree init, tree *idx_next)
{
  if (!gsi_end_p (m_gsi))
    gsi_prev (&m_gsi);
  else
    m_gsi = gsi_last_bb (gsi_bb (m_gsi));
  edge e1 = split_block (gsi_bb (m_gsi), gsi_stmt (m_gsi));
  edge e2 = split_block (e1->dest, (gimple *) NULL);
  edge e3 = make_edge (e1->dest, e1->dest, EDGE_TRUE_VALUE);
  e3->probability = profile_probability::very_unlikely ();
  e2->flags = EDGE_FALSE_VALUE;
  e2->probability = e3->probability.invert ();
  tree idx = make_ssa_name (sizetype);
  gphi *phi = create_phi_node (idx, e1->dest);
  add_phi_arg (phi, init, e1, UNKNOWN_LOCATION);
  *idx_next = make_ssa_name (sizetype);
  add_phi_arg (phi, *idx_next, e3, UNKNOWN_LOCATION);
  m_gsi = gsi_after_labels (e1->dest);
  m_bb = e1->dest;
  m_preheader_bb = e1->src;
  class loop *loop = alloc_loop ();
  loop->header = e1->dest;
  add_loop (loop, e1->src->loop_father);
  return idx;
}

/* Lower large/huge _BitInt statement mergeable or similar STMT which can be
   lowered using iteration from the least significant limb up to the most
   significant limb.  For large _BitInt it is emitted as straight line code
   before current location, for huge _BitInt as a loop handling two limbs
   at once, followed by handling up to limbs in straight line code (at most
   one full and one partial limb).  It can also handle EQ_EXPR/NE_EXPR
   comparisons, in that case CMP_CODE should be the comparison code and
   CMP_OP1/CMP_OP2 the comparison operands.  */

tree
bitint_large_huge::lower_mergeable_stmt (gimple *stmt, tree_code &cmp_code,
					 tree cmp_op1, tree cmp_op2)
{
  bool eq_p = cmp_code != ERROR_MARK;
  tree type;
  if (eq_p)
    type = TREE_TYPE (cmp_op1);
  else
    type = TREE_TYPE (gimple_assign_lhs (stmt));
  gcc_assert (TREE_CODE (type) == BITINT_TYPE);
  bitint_prec_kind kind = bitint_precision_kind (type);
  gcc_assert (kind >= bitint_prec_large);
  gimple *g;
  tree lhs = gimple_get_lhs (stmt);
  tree rhs1, lhs_type = lhs ? TREE_TYPE (lhs) : NULL_TREE;
  if (lhs
      && TREE_CODE (lhs) == SSA_NAME
      && TREE_CODE (TREE_TYPE (lhs)) == BITINT_TYPE
      && bitint_precision_kind (TREE_TYPE (lhs)) >= bitint_prec_large)
    {
      int p = var_to_partition (m_map, lhs);
      gcc_assert (m_vars[p] != NULL_TREE);
      m_lhs = lhs = m_vars[p];
    }
  unsigned cnt, rem = 0, end = 0, prec = TYPE_PRECISION (type);
  bool sext = false;
  tree ext = NULL_TREE, store_operand = NULL_TREE;
  bool eh = false;
  basic_block eh_pad = NULL;
  tree nlhs = NULL_TREE;
  unsigned HOST_WIDE_INT bo_idx = 0;
  unsigned HOST_WIDE_INT bo_bit = 0;
  tree bf_cur = NULL_TREE, bf_next = NULL_TREE;
  if (gimple_store_p (stmt))
    {
      store_operand = gimple_assign_rhs1 (stmt);
      eh = stmt_ends_bb_p (stmt);
      if (eh)
	{
	  edge e;
	  edge_iterator ei;
	  basic_block bb = gimple_bb (stmt);

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (e->flags & EDGE_EH)
	      {
		eh_pad = e->dest;
		break;
	      }
	}
      if (TREE_CODE (lhs) == COMPONENT_REF
	  && DECL_BIT_FIELD_TYPE (TREE_OPERAND (lhs, 1)))
	{
	  tree fld = TREE_OPERAND (lhs, 1);
	  gcc_assert (tree_fits_uhwi_p (DECL_FIELD_BIT_OFFSET (fld)));
	  tree repr = DECL_BIT_FIELD_REPRESENTATIVE (fld);
	  poly_int64 bitoffset;
	  poly_uint64 field_offset, repr_offset;
	  if ((tree_to_uhwi (DECL_FIELD_BIT_OFFSET (fld)) % BITS_PER_UNIT) == 0)
	    nlhs = lhs;
	  else
	    {
	      bool var_field_off = false;
	      if (poly_int_tree_p (DECL_FIELD_OFFSET (fld), &field_offset)
		  && poly_int_tree_p (DECL_FIELD_OFFSET (repr), &repr_offset))
		bitoffset = (field_offset - repr_offset) * BITS_PER_UNIT;
	      else
		{
		  bitoffset = 0;
		  var_field_off = true;
		}
	      bitoffset += (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (fld))
			    - tree_to_uhwi (DECL_FIELD_BIT_OFFSET (repr)));
	      nlhs = build3 (COMPONENT_REF, TREE_TYPE (repr),
			     TREE_OPERAND (lhs, 0), repr,
			     var_field_off
			     ? TREE_OPERAND (lhs, 2) : NULL_TREE);
	      HOST_WIDE_INT bo = bitoffset.to_constant ();
	      bo_idx = (unsigned HOST_WIDE_INT) bo / limb_prec;
	      bo_bit = (unsigned HOST_WIDE_INT) bo % limb_prec;
	    }
	}
    }
  if ((store_operand
       && TREE_CODE (store_operand) == SSA_NAME
       && (m_names == NULL
	   || !bitmap_bit_p (m_names, SSA_NAME_VERSION (store_operand)))
       && gimple_assign_cast_p (SSA_NAME_DEF_STMT (store_operand)))
      || gimple_assign_cast_p (stmt))
    {
      rhs1 = gimple_assign_rhs1 (store_operand
				 ? SSA_NAME_DEF_STMT (store_operand)
				 : stmt);
      if (TREE_CODE (rhs1) == VIEW_CONVERT_EXPR)
	rhs1 = TREE_OPERAND (rhs1, 0);
      /* Optimize mergeable ops ending with widening cast to _BitInt
	 (or followed by store).  We can lower just the limbs of the
	 cast operand and widen afterwards.  */
      if (TREE_CODE (rhs1) == SSA_NAME
	  && (m_names == NULL
	      || !bitmap_bit_p (m_names, SSA_NAME_VERSION (rhs1)))
	  && TREE_CODE (TREE_TYPE (rhs1)) == BITINT_TYPE
	  && bitint_precision_kind (TREE_TYPE (rhs1)) >= bitint_prec_large
	  && (CEIL ((unsigned) TYPE_PRECISION (TREE_TYPE (rhs1)),
		    limb_prec) < CEIL (prec, limb_prec)
	      || (kind == bitint_prec_huge
		  && TYPE_PRECISION (TREE_TYPE (rhs1)) < prec)))
	{
	  store_operand = rhs1;
	  prec = TYPE_PRECISION (TREE_TYPE (rhs1));
	  kind = bitint_precision_kind (TREE_TYPE (rhs1));
	  if (!TYPE_UNSIGNED (TREE_TYPE (rhs1)))
	    sext = true;
	}
    }
  tree idx = NULL_TREE, idx_first = NULL_TREE, idx_next = NULL_TREE;
  if (kind == bitint_prec_large)
    cnt = CEIL (prec, limb_prec);
  else
    {
      rem = (prec % (2 * limb_prec));
      end = (prec - rem) / limb_prec;
      cnt = 2 + CEIL (rem, limb_prec);
      idx = idx_first = create_loop (size_zero_node, &idx_next);
    }

  basic_block edge_bb = NULL;
  if (eq_p)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      gsi_prev (&gsi);
      edge e = split_block (gsi_bb (gsi), gsi_stmt (gsi));
      edge_bb = e->src;
      if (kind == bitint_prec_large)
	m_gsi = gsi_end_bb (edge_bb);
    }
  else
    m_after_stmt = stmt;
  if (kind != bitint_prec_large)
    m_upwards_2limb = end;
  m_upwards = true;

  bool separate_ext
    = (prec != (unsigned) TYPE_PRECISION (type)
       && (CEIL ((unsigned) TYPE_PRECISION (type), limb_prec)
	   > CEIL (prec, limb_prec)));

  for (unsigned i = 0; i < cnt; i++)
    {
      m_data_cnt = 0;
      if (kind == bitint_prec_large)
	idx = size_int (i);
      else if (i >= 2)
	idx = size_int (end + (i > 2));
      if (eq_p)
	{
	  rhs1 = handle_operand (cmp_op1, idx);
	  tree rhs2 = handle_operand (cmp_op2, idx);
	  g = gimple_build_cond (NE_EXPR, rhs1, rhs2, NULL_TREE, NULL_TREE);
	  insert_before (g);
	  edge e1 = split_block (gsi_bb (m_gsi), g);
	  e1->flags = EDGE_FALSE_VALUE;
	  edge e2 = make_edge (e1->src, gimple_bb (stmt), EDGE_TRUE_VALUE);
	  e1->probability = profile_probability::unlikely ();
	  e2->probability = e1->probability.invert ();
	  if (i == 0)
	    set_immediate_dominator (CDI_DOMINATORS, e2->dest, e2->src);
	  m_gsi = gsi_after_labels (e1->dest);
	}
      else
	{
	  if (store_operand)
	    rhs1 = handle_operand (store_operand, idx);
	  else
	    rhs1 = handle_stmt (stmt, idx);
	  if (!useless_type_conversion_p (m_limb_type, TREE_TYPE (rhs1)))
	    rhs1 = add_cast (m_limb_type, rhs1);
	  if (sext && i == cnt - 1)
	    ext = rhs1;
	  tree nidx = idx;
	  if (bo_idx)
	    {
	      if (tree_fits_uhwi_p (idx))
		nidx = size_int (tree_to_uhwi (idx) + bo_idx);
	      else
		{
		  nidx = make_ssa_name (sizetype);
		  g = gimple_build_assign (nidx, PLUS_EXPR, idx,
					   size_int (bo_idx));
		  insert_before (g);
		}
	    }
	  bool done = false;
	  basic_block new_bb = NULL;
	  /* Handle stores into bit-fields.  */
	  if (bo_bit)
	    {
	      if (i == 0)
		{
		  edge e2 = NULL;
		  if (kind != bitint_prec_large)
		    {
		      prepare_data_in_out (build_zero_cst (m_limb_type),
					   idx, &bf_next);
		      bf_next = m_data.pop ();
		      bf_cur = m_data.pop ();
		      g = gimple_build_cond (EQ_EXPR, idx, size_zero_node,
					     NULL_TREE, NULL_TREE);
		      edge edge_true;
		      if_then_else (g, profile_probability::unlikely (),
				    edge_true, e2);
		      new_bb = e2->dest;
		    }
		  tree ftype
		    = build_nonstandard_integer_type (limb_prec - bo_bit, 1);
		  tree bfr = build_bit_field_ref (ftype, unshare_expr (nlhs),
						  limb_prec - bo_bit,
						  bo_idx * limb_prec + bo_bit);
		  tree t = add_cast (ftype, rhs1);
		  g = gimple_build_assign (bfr, t);
		  insert_before (g);
		  if (eh)
		    {
		      maybe_duplicate_eh_stmt (g, stmt);
		      if (eh_pad)
			{
			  edge e = split_block (gsi_bb (m_gsi), g);
			  m_gsi = gsi_after_labels (e->dest);
			  add_eh_edge (e->src,
				       find_edge (gimple_bb (stmt), eh_pad));
			}
		    }
		  if (kind == bitint_prec_large)
		    {
		      bf_cur = rhs1;
		      done = true;
		    }
		  else if (e2)
		    m_gsi = gsi_after_labels (e2->src);
		}
	      if (!done)
		{
		  tree t1 = make_ssa_name (m_limb_type);
		  tree t2 = make_ssa_name (m_limb_type);
		  tree t3 = make_ssa_name (m_limb_type);
		  g = gimple_build_assign (t1, RSHIFT_EXPR, bf_cur,
					   build_int_cst (unsigned_type_node,
							  limb_prec - bo_bit));
		  insert_before (g);
		  g = gimple_build_assign (t2, LSHIFT_EXPR, rhs1,
					   build_int_cst (unsigned_type_node,
							  bo_bit));
		  insert_before (g);
		  bf_cur = rhs1;
		  g = gimple_build_assign (t3, BIT_IOR_EXPR, t1, t2);
		  insert_before (g);
		  rhs1 = t3;
		  if (bf_next && i == 1)
		    {
		      g = gimple_build_assign (bf_next, bf_cur);
		      insert_before (g);
		    }
		}
	    }
	  if (!done)
	    {
	      /* Handle bit-field access to partial last limb if needed.  */
	      if (nlhs
		  && i == cnt - 1
		  && !separate_ext
		  && tree_fits_uhwi_p (idx))
		{
		  unsigned int tprec = TYPE_PRECISION (type);
		  unsigned int rprec = (tprec - 1) % limb_prec + 1;
		  if (rprec + bo_bit < (unsigned) limb_prec)
		    {
		      tree ftype
			= build_nonstandard_integer_type (rprec + bo_bit, 1);
		      tree bfr
			= build_bit_field_ref (ftype, unshare_expr (nlhs),
					       rprec + bo_bit,
					       (bo_idx + tprec / limb_prec)
					       * limb_prec);
		      tree t = add_cast (ftype, rhs1);
		      g = gimple_build_assign (bfr, t);
		      done = true;
		      bf_cur = NULL_TREE;
		    }
		  else if (rprec + bo_bit == (unsigned) limb_prec)
		    bf_cur = NULL_TREE;
		}
	      /* Otherwise, stores to any other lhs.  */
	      if (!done)
		{
		  tree l = limb_access (nlhs ? NULL_TREE : lhs_type,
					nlhs ? nlhs : lhs, nidx, true);
		  g = gimple_build_assign (l, rhs1);
		}
	      insert_before (g);
	      if (eh)
		{
		  maybe_duplicate_eh_stmt (g, stmt);
		  if (eh_pad)
		    {
		      edge e = split_block (gsi_bb (m_gsi), g);
		      m_gsi = gsi_after_labels (e->dest);
		      add_eh_edge (e->src,
				   find_edge (gimple_bb (stmt), eh_pad));
		    }
		}
	      if (new_bb)
		m_gsi = gsi_after_labels (new_bb);
	    }
	}
      m_first = false;
      if (kind == bitint_prec_huge && i <= 1)
	{
	  if (i == 0)
	    {
	      idx = make_ssa_name (sizetype);
	      g = gimple_build_assign (idx, PLUS_EXPR, idx_first,
				       size_one_node);
	      insert_before (g);
	    }
	  else
	    {
	      g = gimple_build_assign (idx_next, PLUS_EXPR, idx_first,
				       size_int (2));
	      insert_before (g);
	      g = gimple_build_cond (NE_EXPR, idx_next, size_int (end),
				     NULL_TREE, NULL_TREE);
	      insert_before (g);
	      if (eq_p)
		m_gsi = gsi_after_labels (edge_bb);
	      else
		m_gsi = gsi_for_stmt (stmt);
	      m_bb = NULL;
	    }
	}
    }

  if (separate_ext)
    {
      if (sext)
	{
	  ext = add_cast (signed_type_for (m_limb_type), ext);
	  tree lpm1 = build_int_cst (unsigned_type_node,
				     limb_prec - 1);
	  tree n = make_ssa_name (TREE_TYPE (ext));
	  g = gimple_build_assign (n, RSHIFT_EXPR, ext, lpm1);
	  insert_before (g);
	  ext = add_cast (m_limb_type, n);
	}
      else
	ext = build_zero_cst (m_limb_type);
      kind = bitint_precision_kind (type);
      unsigned start = CEIL (prec, limb_prec);
      prec = TYPE_PRECISION (type);
      idx = idx_first = idx_next = NULL_TREE;
      if (prec <= (start + 2 + (bo_bit != 0)) * limb_prec)
	kind = bitint_prec_large;
      if (kind == bitint_prec_large)
	cnt = CEIL (prec, limb_prec) - start;
      else
	{
	  rem = prec % limb_prec;
	  end = (prec - rem) / limb_prec;
	  cnt = (bo_bit != 0) + 1 + (rem != 0);
	}
      for (unsigned i = 0; i < cnt; i++)
	{
	  if (kind == bitint_prec_large || (i == 0 && bo_bit != 0))
	    idx = size_int (start + i);
	  else if (i == cnt - 1 && (rem != 0))
	    idx = size_int (end);
	  else if (i == (bo_bit != 0))
	    idx = create_loop (size_int (start + i), &idx_next);
	  rhs1 = ext;
	  if (bf_cur != NULL_TREE && bf_cur != ext)
	    {
	      tree t1 = make_ssa_name (m_limb_type);
	      g = gimple_build_assign (t1, RSHIFT_EXPR, bf_cur,
				       build_int_cst (unsigned_type_node,
						      limb_prec - bo_bit));
	      insert_before (g);
	      if (integer_zerop (ext))
		rhs1 = t1;
	      else
		{
		  tree t2 = make_ssa_name (m_limb_type);
		  rhs1 = make_ssa_name (m_limb_type);
		  g = gimple_build_assign (t2, LSHIFT_EXPR, ext,
					   build_int_cst (unsigned_type_node,
							  bo_bit));
		  insert_before (g);
		  g = gimple_build_assign (rhs1, BIT_IOR_EXPR, t1, t2);
		  insert_before (g);
		}
	      bf_cur = ext;
	    }
	  tree nidx = idx;
	  if (bo_idx)
	    {
	      if (tree_fits_uhwi_p (idx))
		nidx = size_int (tree_to_uhwi (idx) + bo_idx);
	      else
		{
		  nidx = make_ssa_name (sizetype);
		  g = gimple_build_assign (nidx, PLUS_EXPR, idx,
					   size_int (bo_idx));
		  insert_before (g);
		}
	    }
	  bool done = false;
	  /* Handle bit-field access to partial last limb if needed.  */
	  if (nlhs && i == cnt - 1)
	    {
	      unsigned int tprec = TYPE_PRECISION (type);
	      unsigned int rprec = (tprec - 1) % limb_prec + 1;
	      if (rprec + bo_bit < (unsigned) limb_prec)
		{
		  tree ftype
		    = build_nonstandard_integer_type (rprec + bo_bit, 1);
		  tree bfr
		    = build_bit_field_ref (ftype, unshare_expr (nlhs),
					   rprec + bo_bit,
					   (bo_idx + tprec / limb_prec)
					   * limb_prec);
		  tree t = add_cast (ftype, rhs1);
		  g = gimple_build_assign (bfr, t);
		  done = true;
		  bf_cur = NULL_TREE;
		}
	      else if (rprec + bo_bit == (unsigned) limb_prec)
		bf_cur = NULL_TREE;
	    }
	  /* Otherwise, stores to any other lhs.  */
	  if (!done)
	    {
	      tree l = limb_access (nlhs ? NULL_TREE : lhs_type,
				    nlhs ? nlhs : lhs, nidx, true);
	      g = gimple_build_assign (l, rhs1);
	    }
	  insert_before (g);
	  if (eh)
	    {
	      maybe_duplicate_eh_stmt (g, stmt);
	      if (eh_pad)
		{
		  edge e = split_block (gsi_bb (m_gsi), g);
		  m_gsi = gsi_after_labels (e->dest);
		  add_eh_edge (e->src, find_edge (gimple_bb (stmt), eh_pad));
		}
	    }
	  if (kind == bitint_prec_huge && i == (bo_bit != 0))
	    {
	      g = gimple_build_assign (idx_next, PLUS_EXPR, idx,
				       size_one_node);
	      insert_before (g);
	      g = gimple_build_cond (NE_EXPR, idx_next, size_int (end),
				     NULL_TREE, NULL_TREE);
	      insert_before (g);
	      m_gsi = gsi_for_stmt (stmt);
	      m_bb = NULL;
	    }
	}
    }
  if (bf_cur != NULL_TREE)
    {
      unsigned int tprec = TYPE_PRECISION (type);
      unsigned int rprec = (tprec + bo_bit) % limb_prec;
      tree ftype = build_nonstandard_integer_type (rprec, 1);
      tree bfr = build_bit_field_ref (ftype, unshare_expr (nlhs),
				      rprec,
				      (bo_idx + (tprec + bo_bit) / limb_prec)
				      * limb_prec);
      rhs1 = bf_cur;
      if (bf_cur != ext)
	{
	  rhs1 = make_ssa_name (TREE_TYPE (rhs1));
	  g = gimple_build_assign (rhs1, RSHIFT_EXPR, bf_cur,
				   build_int_cst (unsigned_type_node,
						  limb_prec - bo_bit));
	  insert_before (g);
	}
      rhs1 = add_cast (ftype, rhs1);
      g = gimple_build_assign (bfr, rhs1);
      insert_before (g);
      if (eh)
	{
	  maybe_duplicate_eh_stmt (g, stmt);
	  if (eh_pad)
	    {
	      edge e = split_block (gsi_bb (m_gsi), g);
	      m_gsi = gsi_after_labels (e->dest);
	      add_eh_edge (e->src, find_edge (gimple_bb (stmt), eh_pad));
	    }
	}
    }

  if (gimple_store_p (stmt))
    {
      unlink_stmt_vdef (stmt);
      release_ssa_name (gimple_vdef (stmt));
      gsi_remove (&m_gsi, true);
    }
  if (eq_p)
    {
      lhs = make_ssa_name (boolean_type_node);
      basic_block bb = gimple_bb (stmt);
      gphi *phi = create_phi_node (lhs, bb);
      edge e = find_edge (gsi_bb (m_gsi), bb);
      unsigned int n = EDGE_COUNT (bb->preds);
      for (unsigned int i = 0; i < n; i++)
	{
	  edge e2 = EDGE_PRED (bb, i);
	  add_phi_arg (phi, e == e2 ? boolean_true_node : boolean_false_node,
		       e2, UNKNOWN_LOCATION);
	}
      cmp_code = cmp_code == EQ_EXPR ? NE_EXPR : EQ_EXPR;
      return lhs;
    }
  else
    return NULL_TREE;
}

/* Handle a large/huge _BitInt comparison statement STMT other than
   EQ_EXPR/NE_EXPR.  CMP_CODE, CMP_OP1 and CMP_OP2 meaning is like in
   lower_mergeable_stmt.  The {GT,GE,LT,LE}_EXPR comparisons are
   lowered by iteration from the most significant limb downwards to
   the least significant one, for large _BitInt in straight line code,
   otherwise with most significant limb handled in
   straight line code followed by a loop handling one limb at a time.
   Comparisons with unsigned huge _BitInt with precisions which are
   multiples of limb precision can use just the loop and don't need to
   handle most significant limb before the loop.  The loop or straight
   line code jumps to final basic block if a particular pair of limbs
   is not equal.  */

tree
bitint_large_huge::lower_comparison_stmt (gimple *stmt, tree_code &cmp_code,
					  tree cmp_op1, tree cmp_op2)
{
  tree type = TREE_TYPE (cmp_op1);
  gcc_assert (TREE_CODE (type) == BITINT_TYPE);
  bitint_prec_kind kind = bitint_precision_kind (type);
  gcc_assert (kind >= bitint_prec_large);
  gimple *g;
  if (!TYPE_UNSIGNED (type)
      && integer_zerop (cmp_op2)
      && (cmp_code == GE_EXPR || cmp_code == LT_EXPR))
    {
      unsigned end = CEIL ((unsigned) TYPE_PRECISION (type), limb_prec) - 1;
      tree idx = size_int (end);
      m_data_cnt = 0;
      tree rhs1 = handle_operand (cmp_op1, idx);
      if (TYPE_UNSIGNED (TREE_TYPE (rhs1)))
	{
	  tree stype = signed_type_for (TREE_TYPE (rhs1));
	  rhs1 = add_cast (stype, rhs1);
	}
      tree lhs = make_ssa_name (boolean_type_node);
      g = gimple_build_assign (lhs, cmp_code, rhs1,
			       build_zero_cst (TREE_TYPE (rhs1)));
      insert_before (g);
      cmp_code = NE_EXPR;
      return lhs;
    }

  unsigned cnt, rem = 0, end = 0;
  tree idx = NULL_TREE, idx_next = NULL_TREE;
  if (kind == bitint_prec_large)
    cnt = CEIL ((unsigned) TYPE_PRECISION (type), limb_prec);
  else
    {
      rem = ((unsigned) TYPE_PRECISION (type) % limb_prec);
      if (rem == 0 && !TYPE_UNSIGNED (type))
	rem = limb_prec;
      end = ((unsigned) TYPE_PRECISION (type) - rem) / limb_prec;
      cnt = 1 + (rem != 0);
    }

  basic_block edge_bb = NULL;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  gsi_prev (&gsi);
  edge e = split_block (gsi_bb (gsi), gsi_stmt (gsi));
  edge_bb = e->src;
  m_gsi = gsi_end_bb (edge_bb);

  edge *edges = XALLOCAVEC (edge, cnt * 2);
  for (unsigned i = 0; i < cnt; i++)
    {
      m_data_cnt = 0;
      if (kind == bitint_prec_large)
	idx = size_int (cnt - i - 1);
      else if (i == cnt - 1)
	idx = create_loop (size_int (end - 1), &idx_next);
      else
	idx = size_int (end);
      tree rhs1 = handle_operand (cmp_op1, idx);
      tree rhs2 = handle_operand (cmp_op2, idx);
      if (i == 0
	  && !TYPE_UNSIGNED (type)
	  && TYPE_UNSIGNED (TREE_TYPE (rhs1)))
	{
	  tree stype = signed_type_for (TREE_TYPE (rhs1));
	  rhs1 = add_cast (stype, rhs1);
	  rhs2 = add_cast (stype, rhs2);
	}
      g = gimple_build_cond (GT_EXPR, rhs1, rhs2, NULL_TREE, NULL_TREE);
      insert_before (g);
      edge e1 = split_block (gsi_bb (m_gsi), g);
      e1->flags = EDGE_FALSE_VALUE;
      edge e2 = make_edge (e1->src, gimple_bb (stmt), EDGE_TRUE_VALUE);
      e1->probability = profile_probability::likely ();
      e2->probability = e1->probability.invert ();
      if (i == 0)
	set_immediate_dominator (CDI_DOMINATORS, e2->dest, e2->src);
      m_gsi = gsi_after_labels (e1->dest);
      edges[2 * i] = e2;
      g = gimple_build_cond (LT_EXPR, rhs1, rhs2, NULL_TREE, NULL_TREE);
      insert_before (g);
      e1 = split_block (gsi_bb (m_gsi), g);
      e1->flags = EDGE_FALSE_VALUE;
      e2 = make_edge (e1->src, gimple_bb (stmt), EDGE_TRUE_VALUE);
      e1->probability = profile_probability::unlikely ();
      e2->probability = e1->probability.invert ();
      m_gsi = gsi_after_labels (e1->dest);
      edges[2 * i + 1] = e2;
      m_first = false;
      if (kind == bitint_prec_huge && i == cnt - 1)
	{
	  g = gimple_build_assign (idx_next, PLUS_EXPR, idx, size_int (-1));
	  insert_before (g);
	  g = gimple_build_cond (NE_EXPR, idx, size_zero_node,
				 NULL_TREE, NULL_TREE);
	  insert_before (g);
	  edge true_edge, false_edge;
	  extract_true_false_edges_from_block (gsi_bb (m_gsi),
					       &true_edge, &false_edge);
	  m_gsi = gsi_after_labels (false_edge->dest);
	  m_bb = NULL;
	}
    }

  tree lhs = make_ssa_name (boolean_type_node);
  basic_block bb = gimple_bb (stmt);
  gphi *phi = create_phi_node (lhs, bb);
  for (unsigned int i = 0; i < cnt * 2; i++)
    {
      tree val = ((cmp_code == GT_EXPR || cmp_code == GE_EXPR)
		  ^ (i & 1)) ? boolean_true_node : boolean_false_node;
      add_phi_arg (phi, val, edges[i], UNKNOWN_LOCATION);
    }
  add_phi_arg (phi, (cmp_code == GE_EXPR || cmp_code == LE_EXPR)
		    ? boolean_true_node : boolean_false_node,
	       find_edge (gsi_bb (m_gsi), bb), UNKNOWN_LOCATION);
  cmp_code = NE_EXPR;
  return lhs;
}

/* Lower large/huge _BitInt left and right shift except for left
   shift by < limb_prec constant.  */

void
bitint_large_huge::lower_shift_stmt (tree obj, gimple *stmt)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree type = TREE_TYPE (rhs1);
  gimple *final_stmt = gsi_stmt (m_gsi);
  gcc_assert (TREE_CODE (type) == BITINT_TYPE
	      && bitint_precision_kind (type) >= bitint_prec_large);
  int prec = TYPE_PRECISION (type);
  tree n = gimple_assign_rhs2 (stmt), n1, n2, n3, n4;
  gimple *g;
  if (obj == NULL_TREE)
    {
      int part = var_to_partition (m_map, lhs);
      gcc_assert (m_vars[part] != NULL_TREE);
      obj = m_vars[part];
    }
  /* Preparation code common for both left and right shifts.
     unsigned n1 = n % limb_prec;
     size_t n2 = n / limb_prec;
     size_t n3 = n1 != 0;
     unsigned n4 = (limb_prec - n1) % limb_prec;
     (for power of 2 limb_prec n4 can be -n1 & (limb_prec)).  */
  if (TREE_CODE (n) == INTEGER_CST)
    {
      tree lp = build_int_cst (TREE_TYPE (n), limb_prec);
      n1 = int_const_binop (TRUNC_MOD_EXPR, n, lp);
      n2 = fold_convert (sizetype, int_const_binop (TRUNC_DIV_EXPR, n, lp));
      n3 = size_int (!integer_zerop (n1));
      n4 = int_const_binop (TRUNC_MOD_EXPR,
			    int_const_binop (MINUS_EXPR, lp, n1), lp);
    }
  else
    {
      n1 = make_ssa_name (TREE_TYPE (n));
      n2 = make_ssa_name (sizetype);
      n3 = make_ssa_name (sizetype);
      n4 = make_ssa_name (TREE_TYPE (n));
      if (pow2p_hwi (limb_prec))
	{
	  tree lpm1 = build_int_cst (TREE_TYPE (n), limb_prec - 1);
	  g = gimple_build_assign (n1, BIT_AND_EXPR, n, lpm1);
	  insert_before (g);
	  g = gimple_build_assign (useless_type_conversion_p (sizetype,
							      TREE_TYPE (n))
				   ? n2 : make_ssa_name (TREE_TYPE (n)),
				   RSHIFT_EXPR, n,
				   build_int_cst (TREE_TYPE (n),
						  exact_log2 (limb_prec)));
	  insert_before (g);
	  if (gimple_assign_lhs (g) != n2)
	    {
	      g = gimple_build_assign (n2, NOP_EXPR, gimple_assign_lhs (g));
	      insert_before (g);
	    }
	  g = gimple_build_assign (make_ssa_name (TREE_TYPE (n)),
				   NEGATE_EXPR, n1);
	  insert_before (g);
	  g = gimple_build_assign (n4, BIT_AND_EXPR, gimple_assign_lhs (g),
				   lpm1);
	  insert_before (g);
	}
      else
	{
	  tree lp = build_int_cst (TREE_TYPE (n), limb_prec);
	  g = gimple_build_assign (n1, TRUNC_MOD_EXPR, n, lp);
	  insert_before (g);
	  g = gimple_build_assign (useless_type_conversion_p (sizetype,
							      TREE_TYPE (n))
				   ? n2 : make_ssa_name (TREE_TYPE (n)),
				   TRUNC_DIV_EXPR, n, lp);
	  insert_before (g);
	  if (gimple_assign_lhs (g) != n2)
	    {
	      g = gimple_build_assign (n2, NOP_EXPR, gimple_assign_lhs (g));
	      insert_before (g);
	    }
	  g = gimple_build_assign (make_ssa_name (TREE_TYPE (n)),
				   MINUS_EXPR, lp, n1);
	  insert_before (g);
	  g = gimple_build_assign (n4, TRUNC_MOD_EXPR, gimple_assign_lhs (g),
				   lp);
	  insert_before (g);
	}
      g = gimple_build_assign (make_ssa_name (boolean_type_node), NE_EXPR, n1,
			       build_zero_cst (TREE_TYPE (n)));
      insert_before (g);
      g = gimple_build_assign (n3, NOP_EXPR, gimple_assign_lhs (g));
      insert_before (g);
    }
  tree p = build_int_cst (sizetype,
			  prec / limb_prec - (prec % limb_prec == 0));
  if (rhs_code == RSHIFT_EXPR)
    {
      /* Lower
	   dst = src >> n;
	 as
	   unsigned n1 = n % limb_prec;
	   size_t n2 = n / limb_prec;
	   size_t n3 = n1 != 0;
	   unsigned n4 = (limb_prec - n1) % limb_prec;
	   size_t idx;
	   size_t p = prec / limb_prec - (prec % limb_prec == 0);
	   int signed_p = (typeof (src) -1) < 0;
	   for (idx = n2; idx < ((!signed_p && (prec % limb_prec == 0))
				 ? p : p - n3); ++idx)
	     dst[idx - n2] = (src[idx] >> n1) | (src[idx + n3] << n4);
	   limb_type ext;
	   if (prec % limb_prec == 0)
	     ext = src[p];
	   else if (signed_p)
	     ext = ((signed limb_type) (src[p] << (limb_prec
						   - (prec % limb_prec))))
		   >> (limb_prec - (prec % limb_prec));
	   else
	     ext = src[p] & (((limb_type) 1 << (prec % limb_prec)) - 1);
	   if (!signed_p && (prec % limb_prec == 0))
	     ;
	   else if (idx < prec / 64)
	     {
	       dst[idx - n2] = (src[idx] >> n1) | (ext << n4);
	       ++idx;
	     }
	   idx -= n2;
	   if (signed_p)
	     {
	       dst[idx] = ((signed limb_type) ext) >> n1;
	       ext = ((signed limb_type) ext) >> (limb_prec - 1);
	     }
	   else
	     {
	       dst[idx] = ext >> n1;
	       ext = 0;
	     }
	   for (++idx; idx <= p; ++idx)
	     dst[idx] = ext;  */
      tree pmn3;
      if (TYPE_UNSIGNED (type) && prec % limb_prec == 0)
	pmn3 = p;
      else if (TREE_CODE (n3) == INTEGER_CST)
	pmn3 = int_const_binop (MINUS_EXPR, p, n3);
      else
	{
	  pmn3 = make_ssa_name (sizetype);
	  g = gimple_build_assign (pmn3, MINUS_EXPR, p, n3);
	  insert_before (g);
	}
      g = gimple_build_cond (LT_EXPR, n2, pmn3, NULL_TREE, NULL_TREE);
      edge edge_true, edge_false;
      if_then (g, profile_probability::likely (), edge_true, edge_false);
      tree idx_next;
      tree idx = create_loop (n2, &idx_next);
      tree idxmn2 = make_ssa_name (sizetype);
      tree idxpn3 = make_ssa_name (sizetype);
      g = gimple_build_assign (idxmn2, MINUS_EXPR, idx, n2);
      insert_before (g);
      g = gimple_build_assign (idxpn3, PLUS_EXPR, idx, n3);
      insert_before (g);
      m_data_cnt = 0;
      tree t1 = handle_operand (rhs1, idx);
      m_first = false;
      g = gimple_build_assign (make_ssa_name (m_limb_type),
			       RSHIFT_EXPR, t1, n1);
      insert_before (g);
      t1 = gimple_assign_lhs (g);
      if (!integer_zerop (n3))
	{
	  m_data_cnt = 0;
	  tree t2 = handle_operand (rhs1, idxpn3);
	  g = gimple_build_assign (make_ssa_name (m_limb_type),
				   LSHIFT_EXPR, t2, n4);
	  insert_before (g);
	  t2 = gimple_assign_lhs (g);
	  g = gimple_build_assign (make_ssa_name (m_limb_type),
				   BIT_IOR_EXPR, t1, t2);
	  insert_before (g);
	  t1 = gimple_assign_lhs (g);
	}
      tree l = limb_access (TREE_TYPE (lhs), obj, idxmn2, true);
      g = gimple_build_assign (l, t1);
      insert_before (g);
      g = gimple_build_assign (idx_next, PLUS_EXPR, idx, size_one_node);
      insert_before (g);
      g = gimple_build_cond (LT_EXPR, idx_next, pmn3, NULL_TREE, NULL_TREE);
      insert_before (g);
      idx = make_ssa_name (sizetype);
      m_gsi = gsi_for_stmt (final_stmt);
      gphi *phi = create_phi_node (idx, gsi_bb (m_gsi));
      edge_false = find_edge (edge_false->src, gsi_bb (m_gsi));
      edge_true = EDGE_PRED (gsi_bb (m_gsi),
			     EDGE_PRED (gsi_bb (m_gsi), 0) == edge_false);
      add_phi_arg (phi, n2, edge_false, UNKNOWN_LOCATION);
      add_phi_arg (phi, idx_next, edge_true, UNKNOWN_LOCATION);
      m_data_cnt = 0;
      tree ms = handle_operand (rhs1, p);
      tree ext = ms;
      if (!types_compatible_p (TREE_TYPE (ms), m_limb_type))
	ext = add_cast (m_limb_type, ms);
      if (!(TYPE_UNSIGNED (type) && prec % limb_prec == 0)
	  && !integer_zerop (n3))
	{
	  g = gimple_build_cond (LT_EXPR, idx, p, NULL_TREE, NULL_TREE);
	  if_then (g, profile_probability::likely (), edge_true, edge_false);
	  m_data_cnt = 0;
	  t1 = handle_operand (rhs1, idx);
	  g = gimple_build_assign (make_ssa_name (m_limb_type),
				   RSHIFT_EXPR, t1, n1);
	  insert_before (g);
	  t1 = gimple_assign_lhs (g);
	  g = gimple_build_assign (make_ssa_name (m_limb_type),
				   LSHIFT_EXPR, ext, n4);
	  insert_before (g);
	  tree t2 = gimple_assign_lhs (g);
	  g = gimple_build_assign (make_ssa_name (m_limb_type),
				   BIT_IOR_EXPR, t1, t2);
	  insert_before (g);
	  t1 = gimple_assign_lhs (g);
	  idxmn2 = make_ssa_name (sizetype);
	  g = gimple_build_assign (idxmn2, MINUS_EXPR, idx, n2);
	  insert_before (g);
	  l = limb_access (TREE_TYPE (lhs), obj, idxmn2, true);
	  g = gimple_build_assign (l, t1);
	  insert_before (g);
	  idx_next = make_ssa_name (sizetype);
	  g = gimple_build_assign (idx_next, PLUS_EXPR, idx, size_one_node);
	  insert_before (g);
	  m_gsi = gsi_for_stmt (final_stmt);
	  tree nidx = make_ssa_name (sizetype);
	  phi = create_phi_node (nidx, gsi_bb (m_gsi));
	  edge_false = find_edge (edge_false->src, gsi_bb (m_gsi));
	  edge_true = EDGE_PRED (gsi_bb (m_gsi),
				 EDGE_PRED (gsi_bb (m_gsi), 0) == edge_false);
	  add_phi_arg (phi, idx, edge_false, UNKNOWN_LOCATION);
	  add_phi_arg (phi, idx_next, edge_true, UNKNOWN_LOCATION);
	  idx = nidx;
	}
      g = gimple_build_assign (make_ssa_name (sizetype), MINUS_EXPR, idx, n2);
      insert_before (g);
      idx = gimple_assign_lhs (g);
      tree sext = ext;
      if (!TYPE_UNSIGNED (type))
	sext = add_cast (signed_type_for (m_limb_type), ext);
      g = gimple_build_assign (make_ssa_name (TREE_TYPE (sext)),
			       RSHIFT_EXPR, sext, n1);
      insert_before (g);
      t1 = gimple_assign_lhs (g);
      if (!TYPE_UNSIGNED (type))
	{
	  t1 = add_cast (m_limb_type, t1);
	  g = gimple_build_assign (make_ssa_name (TREE_TYPE (sext)),
				   RSHIFT_EXPR, sext,
				   build_int_cst (TREE_TYPE (n),
						  limb_prec - 1));
	  insert_before (g);
	  ext = add_cast (m_limb_type, gimple_assign_lhs (g));
	}
      else
	ext = build_zero_cst (m_limb_type);
      l = limb_access (TREE_TYPE (lhs), obj, idx, true);
      g = gimple_build_assign (l, t1);
      insert_before (g);
      g = gimple_build_assign (make_ssa_name (sizetype), PLUS_EXPR, idx,
			       size_one_node);
      insert_before (g);
      idx = gimple_assign_lhs (g);
      g = gimple_build_cond (LE_EXPR, idx, p, NULL_TREE, NULL_TREE);
      if_then (g, profile_probability::likely (), edge_true, edge_false);
      idx = create_loop (idx, &idx_next);
      l = limb_access (TREE_TYPE (lhs), obj, idx, true);
      g = gimple_build_assign (l, ext);
      insert_before (g);
      g = gimple_build_assign (idx_next, PLUS_EXPR, idx, size_one_node);
      insert_before (g);
      g = gimple_build_cond (LE_EXPR, idx_next, p, NULL_TREE, NULL_TREE);
      insert_before (g);
    }
  else
    {
      /* Lower
	   dst = src << n;
	 as
	   unsigned n1 = n % limb_prec;
	   size_t n2 = n / limb_prec;
	   size_t n3 = n1 != 0;
	   unsigned n4 = (limb_prec - n1) % limb_prec;
	   size_t idx;
	   size_t p = prec / limb_prec - (prec % limb_prec == 0);
	   for (idx = p; (ssize_t) idx >= (ssize_t) (n2 + n3); --idx)
	     dst[idx] = (src[idx - n2] << n1) | (src[idx - n2 - n3] >> n4);
	   if (n1)
	     {
	       dst[idx] = src[idx - n2] << n1;
	       --idx;
	     }
	   for (; (ssize_t) idx >= 0; --idx)
	     dst[idx] = 0;  */
      tree n2pn3;
      if (TREE_CODE (n2) == INTEGER_CST && TREE_CODE (n3) == INTEGER_CST)
	n2pn3 = int_const_binop (PLUS_EXPR, n2, n3);
      else
	{
	  n2pn3 = make_ssa_name (sizetype);
	  g = gimple_build_assign (n2pn3, PLUS_EXPR, n2, n3);
	  insert_before (g);
	}
      /* For LSHIFT_EXPR, we can use handle_operand with non-INTEGER_CST
	 idx even to access the most significant partial limb.  */
      m_var_msb = true;
      if (integer_zerop (n3))
	/* For n3 == 0 p >= n2 + n3 is always true for all valid shift
	   counts.  Emit if (true) condition that can be optimized later.  */
	g = gimple_build_cond (NE_EXPR, boolean_true_node, boolean_false_node,
			       NULL_TREE, NULL_TREE);
      else
	g = gimple_build_cond (LE_EXPR, n2pn3, p, NULL_TREE, NULL_TREE);
      edge edge_true, edge_false;
      if_then (g, profile_probability::likely (), edge_true, edge_false);
      tree idx_next;
      tree idx = create_loop (p, &idx_next);
      tree idxmn2 = make_ssa_name (sizetype);
      tree idxmn2mn3 = make_ssa_name (sizetype);
      g = gimple_build_assign (idxmn2, MINUS_EXPR, idx, n2);
      insert_before (g);
      g = gimple_build_assign (idxmn2mn3, MINUS_EXPR, idxmn2, n3);
      insert_before (g);
      m_data_cnt = 0;
      tree t1 = handle_operand (rhs1, idxmn2);
      m_first = false;
      g = gimple_build_assign (make_ssa_name (m_limb_type),
			       LSHIFT_EXPR, t1, n1);
      insert_before (g);
      t1 = gimple_assign_lhs (g);
      if (!integer_zerop (n3))
	{
	  m_data_cnt = 0;
	  tree t2 = handle_operand (rhs1, idxmn2mn3);
	  g = gimple_build_assign (make_ssa_name (m_limb_type),
				   RSHIFT_EXPR, t2, n4);
	  insert_before (g);
	  t2 = gimple_assign_lhs (g);
	  g = gimple_build_assign (make_ssa_name (m_limb_type),
				   BIT_IOR_EXPR, t1, t2);
	  insert_before (g);
	  t1 = gimple_assign_lhs (g);
	}
      tree l = limb_access (TREE_TYPE (lhs), obj, idx, true);
      g = gimple_build_assign (l, t1);
      insert_before (g);
      g = gimple_build_assign (idx_next, PLUS_EXPR, idx, size_int (-1));
      insert_before (g);
      tree sn2pn3 = add_cast (ssizetype, n2pn3);
      g = gimple_build_cond (GE_EXPR, add_cast (ssizetype, idx_next), sn2pn3,
			     NULL_TREE, NULL_TREE);
      insert_before (g);
      idx = make_ssa_name (sizetype);
      m_gsi = gsi_for_stmt (final_stmt);
      gphi *phi = create_phi_node (idx, gsi_bb (m_gsi));
      edge_false = find_edge (edge_false->src, gsi_bb (m_gsi));
      edge_true = EDGE_PRED (gsi_bb (m_gsi),
			     EDGE_PRED (gsi_bb (m_gsi), 0) == edge_false);
      add_phi_arg (phi, p, edge_false, UNKNOWN_LOCATION);
      add_phi_arg (phi, idx_next, edge_true, UNKNOWN_LOCATION);
      m_data_cnt = 0;
      if (!integer_zerop (n3))
	{
	  g = gimple_build_cond (NE_EXPR, n3, size_zero_node,
				 NULL_TREE, NULL_TREE);
	  if_then (g, profile_probability::likely (), edge_true, edge_false);
	  idxmn2 = make_ssa_name (sizetype);
	  g = gimple_build_assign (idxmn2, MINUS_EXPR, idx, n2);
	  insert_before (g);
	  m_data_cnt = 0;
	  t1 = handle_operand (rhs1, idxmn2);
	  g = gimple_build_assign (make_ssa_name (m_limb_type),
				   LSHIFT_EXPR, t1, n1);
	  insert_before (g);
	  t1 = gimple_assign_lhs (g);
	  l = limb_access (TREE_TYPE (lhs), obj, idx, true);
	  g = gimple_build_assign (l, t1);
	  insert_before (g);
	  idx_next = make_ssa_name (sizetype);
	  g = gimple_build_assign (idx_next, PLUS_EXPR, idx, size_int (-1));
	  insert_before (g);
	  m_gsi = gsi_for_stmt (final_stmt);
	  tree nidx = make_ssa_name (sizetype);
	  phi = create_phi_node (nidx, gsi_bb (m_gsi));
	  edge_false = find_edge (edge_false->src, gsi_bb (m_gsi));
	  edge_true = EDGE_PRED (gsi_bb (m_gsi),
				 EDGE_PRED (gsi_bb (m_gsi), 0) == edge_false);
	  add_phi_arg (phi, idx, edge_false, UNKNOWN_LOCATION);
	  add_phi_arg (phi, idx_next, edge_true, UNKNOWN_LOCATION);
	  idx = nidx;
	}
      g = gimple_build_cond (GE_EXPR, add_cast (ssizetype, idx),
			     ssize_int (0), NULL_TREE, NULL_TREE);
      if_then (g, profile_probability::likely (), edge_true, edge_false);
      idx = create_loop (idx, &idx_next);
      l = limb_access (TREE_TYPE (lhs), obj, idx, true);
      g = gimple_build_assign (l, build_zero_cst (m_limb_type));
      insert_before (g);
      g = gimple_build_assign (idx_next, PLUS_EXPR, idx, size_int (-1));
      insert_before (g);
      g = gimple_build_cond (GE_EXPR, add_cast (ssizetype, idx_next),
			     ssize_int (0), NULL_TREE, NULL_TREE);
      insert_before (g);
    }
}

/* Lower large/huge _BitInt multiplication or division.  */

void
bitint_large_huge::lower_muldiv_stmt (tree obj, gimple *stmt)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree type = TREE_TYPE (rhs1);
  gcc_assert (TREE_CODE (type) == BITINT_TYPE
	      && bitint_precision_kind (type) >= bitint_prec_large);
  int prec = TYPE_PRECISION (type), prec1, prec2;
  rhs1 = handle_operand_addr (rhs1, stmt, NULL, &prec1);
  rhs2 = handle_operand_addr (rhs2, stmt, NULL, &prec2);
  if (obj == NULL_TREE)
    {
      int part = var_to_partition (m_map, lhs);
      gcc_assert (m_vars[part] != NULL_TREE);
      obj = m_vars[part];
      lhs = build_fold_addr_expr (obj);
    }
  else
    {
      lhs = build_fold_addr_expr (obj);
      lhs = force_gimple_operand_gsi (&m_gsi, lhs, true,
				      NULL_TREE, true, GSI_SAME_STMT);
    }
  tree sitype = lang_hooks.types.type_for_mode (SImode, 0);
  gimple *g;
  switch (rhs_code)
    {
    case MULT_EXPR:
      g = gimple_build_call_internal (IFN_MULBITINT, 6,
				      lhs, build_int_cst (sitype, prec),
				      rhs1, build_int_cst (sitype, prec1),
				      rhs2, build_int_cst (sitype, prec2));
      insert_before (g);
      break;
    case TRUNC_DIV_EXPR:
      g = gimple_build_call_internal (IFN_DIVMODBITINT, 8,
				      lhs, build_int_cst (sitype, prec),
				      null_pointer_node,
				      build_int_cst (sitype, 0),
				      rhs1, build_int_cst (sitype, prec1),
				      rhs2, build_int_cst (sitype, prec2));
      if (!stmt_ends_bb_p (stmt))
	gimple_call_set_nothrow (as_a <gcall *> (g), true);
      insert_before (g);
      break;
    case TRUNC_MOD_EXPR:
      g = gimple_build_call_internal (IFN_DIVMODBITINT, 8, null_pointer_node,
				      build_int_cst (sitype, 0),
				      lhs, build_int_cst (sitype, prec),
				      rhs1, build_int_cst (sitype, prec1),
				      rhs2, build_int_cst (sitype, prec2));
      if (!stmt_ends_bb_p (stmt))
	gimple_call_set_nothrow (as_a <gcall *> (g), true);
      insert_before (g);
      break;
    default:
      gcc_unreachable ();
    }
  if (stmt_ends_bb_p (stmt))
    {
      maybe_duplicate_eh_stmt (g, stmt);
      edge e1;
      edge_iterator ei;
      basic_block bb = gimple_bb (stmt);

      FOR_EACH_EDGE (e1, ei, bb->succs)
	if (e1->flags & EDGE_EH)
	  break;
      if (e1)
	{
	  edge e2 = split_block (gsi_bb (m_gsi), g);
	  m_gsi = gsi_after_labels (e2->dest);
	  add_eh_edge (e2->src, e1);
	}
    }
}

/* Lower large/huge _BitInt conversion to/from floating point.  */

void
bitint_large_huge::lower_float_conv_stmt (tree obj, gimple *stmt)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree sitype = lang_hooks.types.type_for_mode (SImode, 0);
  gimple *g;
  if (rhs_code == FIX_TRUNC_EXPR)
    {
      int prec = TYPE_PRECISION (TREE_TYPE (lhs));
      if (!TYPE_UNSIGNED (TREE_TYPE (lhs)))
	prec = -prec;
      if (obj == NULL_TREE)
	{
	  int part = var_to_partition (m_map, lhs);
	  gcc_assert (m_vars[part] != NULL_TREE);
	  obj = m_vars[part];
	  lhs = build_fold_addr_expr (obj);
	}
      else
	{
	  lhs = build_fold_addr_expr (obj);
	  lhs = force_gimple_operand_gsi (&m_gsi, lhs, true,
					  NULL_TREE, true, GSI_SAME_STMT);
	}
      scalar_mode from_mode
	= as_a <scalar_mode> (TYPE_MODE (TREE_TYPE (rhs1)));
#ifdef HAVE_SFmode
      /* IEEE single is a full superset of both IEEE half and
	 bfloat formats, convert to float first and then to _BitInt
	 to avoid the need of another 2 library routines.  */
      if ((REAL_MODE_FORMAT (from_mode) == &arm_bfloat_half_format
	   || REAL_MODE_FORMAT (from_mode) == &ieee_half_format)
	  && REAL_MODE_FORMAT (SFmode) == &ieee_single_format)
	{
	  tree type = lang_hooks.types.type_for_mode (SFmode, 0);
	  if (type)
	    rhs1 = add_cast (type, rhs1);
	}
#endif
      g = gimple_build_call_internal (IFN_FLOATTOBITINT, 3,
				      lhs, build_int_cst (sitype, prec),
				      rhs1);
      insert_before (g);
    }
  else
    {
      int prec;
      rhs1 = handle_operand_addr (rhs1, stmt, NULL, &prec);
      g = gimple_build_call_internal (IFN_BITINTTOFLOAT, 2,
				      rhs1, build_int_cst (sitype, prec));
      gimple_call_set_lhs (g, lhs);
      if (!stmt_ends_bb_p (stmt))
	gimple_call_set_nothrow (as_a <gcall *> (g), true);
      gsi_replace (&m_gsi, g, true);
    }
}

/* Helper method for lower_addsub_overflow and lower_mul_overflow.
   If check_zero is true, caller wants to check if all bits in [start, end)
   are zero, otherwise if bits in [start, end) are either all zero or
   all ones.  L is the limb with index LIMB, START and END are measured
   in bits.  */

tree
bitint_large_huge::arith_overflow_extract_bits (unsigned int start,
						unsigned int end, tree l,
						unsigned int limb,
						bool check_zero)
{
  unsigned startlimb = start / limb_prec;
  unsigned endlimb = (end - 1) / limb_prec;
  gimple *g;

  if ((start % limb_prec) == 0 && (end % limb_prec) == 0)
    return l;
  if (startlimb == endlimb && limb == startlimb)
    {
      if (check_zero)
	{
	  wide_int w = wi::shifted_mask (start % limb_prec,
					 end - start, false, limb_prec);
	  g = gimple_build_assign (make_ssa_name (m_limb_type),
				   BIT_AND_EXPR, l,
				   wide_int_to_tree (m_limb_type, w));
	  insert_before (g);
	  return gimple_assign_lhs (g);
	}
      unsigned int shift = start % limb_prec;
      if ((end % limb_prec) != 0)
	{
	  unsigned int lshift = (-end) % limb_prec;
	  shift += lshift;
	  g = gimple_build_assign (make_ssa_name (m_limb_type),
				   LSHIFT_EXPR, l,
				   build_int_cst (unsigned_type_node,
						  lshift));
	  insert_before (g);
	  l = gimple_assign_lhs (g);
	}
      l = add_cast (signed_type_for (m_limb_type), l);
      g = gimple_build_assign (make_ssa_name (TREE_TYPE (l)),
			       RSHIFT_EXPR, l,
			       build_int_cst (unsigned_type_node, shift));
      insert_before (g);
      return add_cast (m_limb_type, gimple_assign_lhs (g));
    }
  else if (limb == startlimb)
    {
      if ((start % limb_prec) == 0)
	return l;
      if (!check_zero)
	l = add_cast (signed_type_for (m_limb_type), l);
      g = gimple_build_assign (make_ssa_name (TREE_TYPE (l)),
			       RSHIFT_EXPR, l,
			       build_int_cst (unsigned_type_node,
					      start % limb_prec));
      insert_before (g);
      l = gimple_assign_lhs (g);
      if (!check_zero)
	l = add_cast (m_limb_type, l);
      return l;
    }
  else if (limb == endlimb)
    {
      if ((end % limb_prec) == 0)
	return l;
      if (check_zero)
	{
	  wide_int w = wi::mask (end % limb_prec, false, limb_prec);
	  g = gimple_build_assign (make_ssa_name (m_limb_type),
				   BIT_AND_EXPR, l,
				   wide_int_to_tree (m_limb_type, w));
	  insert_before (g);
	  return gimple_assign_lhs (g);
	}
      unsigned int shift = (-end) % limb_prec;
      g = gimple_build_assign (make_ssa_name (m_limb_type),
			       LSHIFT_EXPR, l,
			       build_int_cst (unsigned_type_node, shift));
      insert_before (g);
      l = add_cast (signed_type_for (m_limb_type), gimple_assign_lhs (g));
      g = gimple_build_assign (make_ssa_name (TREE_TYPE (l)),
			       RSHIFT_EXPR, l,
			       build_int_cst (unsigned_type_node, shift));
      insert_before (g);
      return add_cast (m_limb_type, gimple_assign_lhs (g));
    }
  return l;
}

/* Helper method for lower_addsub_overflow and lower_mul_overflow.  Store
   result including overflow flag into the right locations.  */

void
bitint_large_huge::finish_arith_overflow (tree var, tree obj, tree type,
					  tree ovf, tree lhs, tree orig_obj,
					  gimple *stmt, tree_code code)
{
  gimple *g;

  if (obj == NULL_TREE
      && (TREE_CODE (type) != BITINT_TYPE
	  || bitint_precision_kind (type) < bitint_prec_large))
    {
      /* Add support for 3 or more limbs filled in from normal integral
	 type if this assert fails.  If no target chooses limb mode smaller
	 than half of largest supported normal integral type, this will not
	 be needed.  */
      gcc_assert (TYPE_PRECISION (type) <= 2 * limb_prec);
      tree lhs_type = type;
      if (TREE_CODE (type) == BITINT_TYPE
	  && bitint_precision_kind (type) == bitint_prec_middle)
	lhs_type = build_nonstandard_integer_type (TYPE_PRECISION (type),
						   TYPE_UNSIGNED (type));
      tree r1 = limb_access (NULL_TREE, var, size_int (0), true);
      g = gimple_build_assign (make_ssa_name (m_limb_type), r1);
      insert_before (g);
      r1 = gimple_assign_lhs (g);
      if (!useless_type_conversion_p (lhs_type, TREE_TYPE (r1)))
	r1 = add_cast (lhs_type, r1);
      if (TYPE_PRECISION (lhs_type) > limb_prec)
	{
	  tree r2 = limb_access (NULL_TREE, var, size_int (1), true);
	  g = gimple_build_assign (make_ssa_name (m_limb_type), r2);
	  insert_before (g);
	  r2 = gimple_assign_lhs (g);
	  r2 = add_cast (lhs_type, r2);
	  g = gimple_build_assign (make_ssa_name (lhs_type), LSHIFT_EXPR, r2,
				   build_int_cst (unsigned_type_node,
						  limb_prec));
	  insert_before (g);
	  g = gimple_build_assign (make_ssa_name (lhs_type), BIT_IOR_EXPR, r1,
				   gimple_assign_lhs (g));
	  insert_before (g);
	  r1 = gimple_assign_lhs (g);
	}
      if (lhs_type != type)
	r1 = add_cast (type, r1);
      ovf = add_cast (lhs_type, ovf);
      if (lhs_type != type)
	ovf = add_cast (type, ovf);
      g = gimple_build_assign (lhs, COMPLEX_EXPR, r1, ovf);
      m_gsi = gsi_for_stmt (stmt);
      gsi_replace (&m_gsi, g, true);
    }
  else
    {
      unsigned HOST_WIDE_INT nelts = 0;
      tree atype = NULL_TREE;
      if (obj)
	{
	  nelts = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (obj))) / limb_prec;
	  if (orig_obj == NULL_TREE)
	    nelts >>= 1;
	  atype = build_array_type_nelts (m_limb_type, nelts);
	}
      if (var && obj)
	{
	  tree v1, v2;
	  tree zero;
	  if (orig_obj == NULL_TREE)
	    {
	      zero = build_zero_cst (build_pointer_type (TREE_TYPE (obj)));
	      v1 = build2 (MEM_REF, atype,
			   build_fold_addr_expr (unshare_expr (obj)), zero);
	    }
	  else if (!useless_type_conversion_p (atype, TREE_TYPE (obj)))
	    v1 = build1 (VIEW_CONVERT_EXPR, atype, unshare_expr (obj));
	  else
	    v1 = unshare_expr (obj);
	  zero = build_zero_cst (build_pointer_type (TREE_TYPE (var)));
	  v2 = build2 (MEM_REF, atype, build_fold_addr_expr (var), zero);
	  g = gimple_build_assign (v1, v2);
	  insert_before (g);
	}
      if (orig_obj == NULL_TREE && obj)
	{
	  ovf = add_cast (m_limb_type, ovf);
	  tree l = limb_access (NULL_TREE, obj, size_int (nelts), true);
	  g = gimple_build_assign (l, ovf);
	  insert_before (g);
	  if (nelts > 1)
	    {
	      atype = build_array_type_nelts (m_limb_type, nelts - 1);
	      tree off = build_int_cst (build_pointer_type (TREE_TYPE (obj)),
					(nelts + 1) * m_limb_size);
	      tree v1 = build2 (MEM_REF, atype,
				build_fold_addr_expr (unshare_expr (obj)),
				off);
	      g = gimple_build_assign (v1, build_zero_cst (atype));
	      insert_before (g);
	    }
	}
      else if (TREE_CODE (TREE_TYPE (lhs)) == COMPLEX_TYPE)
	{
	  imm_use_iterator ui;
	  use_operand_p use_p;
	  FOR_EACH_IMM_USE_FAST (use_p, ui, lhs)
	    {
	      g = USE_STMT (use_p);
	      if (!is_gimple_assign (g)
		  || gimple_assign_rhs_code (g) != IMAGPART_EXPR)
		continue;
	      tree lhs2 = gimple_assign_lhs (g);
	      gimple *use_stmt;
	      single_imm_use (lhs2, &use_p, &use_stmt);
	      lhs2 = gimple_assign_lhs (use_stmt);
	      gimple_stmt_iterator gsi = gsi_for_stmt (use_stmt);
	      if (useless_type_conversion_p (TREE_TYPE (lhs2), TREE_TYPE (ovf)))
		g = gimple_build_assign (lhs2, ovf);
	      else
		g = gimple_build_assign (lhs2, NOP_EXPR, ovf);
	      gsi_replace (&gsi, g, true);
	      if (gsi_stmt (m_gsi) == use_stmt)
		m_gsi = gsi_for_stmt (g);
	      break;
	    }
	}
      else if (ovf != boolean_false_node)
	{
	  g = gimple_build_cond (NE_EXPR, ovf, boolean_false_node,
				 NULL_TREE, NULL_TREE);
	  edge edge_true, edge_false;
	  if_then (g, profile_probability::very_unlikely (),
		   edge_true, edge_false);
	  tree zero = build_zero_cst (TREE_TYPE (lhs));
	  tree fn = ubsan_build_overflow_builtin (code, m_loc,
						  TREE_TYPE (lhs),
						  zero, zero, NULL);
	  force_gimple_operand_gsi (&m_gsi, fn, true, NULL_TREE,
				    true, GSI_SAME_STMT);
	  m_gsi = gsi_after_labels (edge_true->dest);
	}
    }
  if (var)
    {
      tree clobber = build_clobber (TREE_TYPE (var), CLOBBER_STORAGE_END);
      g = gimple_build_assign (var, clobber);
      gsi_insert_after (&m_gsi, g, GSI_SAME_STMT);
    }
}

/* Helper function for lower_addsub_overflow and lower_mul_overflow.
   Given precisions of result TYPE (PREC), argument 0 precision PREC0,
   argument 1 precision PREC1 and minimum precision for the result
   PREC2, compute *START, *END, *CHECK_ZERO and return OVF.  */

static tree
arith_overflow (tree_code code, tree type, int prec, int prec0, int prec1,
		int prec2, unsigned *start, unsigned *end, bool *check_zero)
{
  *start = 0;
  *end = 0;
  *check_zero = true;
  /* Ignore this special rule for subtraction, even if both
     prec0 >= 0 and prec1 >= 0, their subtraction can be negative
     in infinite precision.  */
  if (code != MINUS_EXPR && prec0 >= 0 && prec1 >= 0)
    {
      /* Result in [0, prec2) is unsigned, if prec > prec2,
	 all bits above it will be zero.  */
      if ((prec - !TYPE_UNSIGNED (type)) >= prec2)
	return boolean_false_node;
      else
	{
	  /* ovf if any of bits in [start, end) is non-zero.  */
	  *start = prec - !TYPE_UNSIGNED (type);
	  *end = prec2;
	}
    }
  else if (TYPE_UNSIGNED (type))
    {
      /* If result in [0, prec2) is signed and if prec > prec2,
	 all bits above it will be sign bit copies.  */
      if (prec >= prec2)
	{
	  /* ovf if bit prec - 1 is non-zero.  */
	  *start = prec - 1;
	  *end = prec;
	}
      else
	{
	  /* ovf if any of bits in [start, end) is non-zero.  */
	  *start = prec;
	  *end = prec2;
	}
    }
  else if (prec >= prec2)
    return boolean_false_node;
  else
    {
      /* ovf if [start, end) bits aren't all zeros or all ones.  */
      *start = prec - 1;
      *end = prec2;
      *check_zero = false;
    }
  return NULL_TREE;
}

/* Lower a .{ADD,SUB}_OVERFLOW call with at least one large/huge _BitInt
   argument or return type _Complex large/huge _BitInt.  */

void
bitint_large_huge::lower_addsub_overflow (tree obj, gimple *stmt)
{
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  tree lhs = gimple_call_lhs (stmt);
  gimple *g;

  if (!lhs)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      gsi_remove (&gsi, true);
      return;
    }
  gimple *final_stmt = gsi_stmt (m_gsi);
  tree type = TREE_TYPE (lhs);
  if (TREE_CODE (type) == COMPLEX_TYPE)
    type = TREE_TYPE (type);
  int prec = TYPE_PRECISION (type);
  int prec0 = range_to_prec (arg0, stmt);
  int prec1 = range_to_prec (arg1, stmt);
  /* If PREC0 >= 0 && PREC1 >= 0 and CODE is not MINUS_EXPR, PREC2 is
     the be minimum unsigned precision of any possible operation's
     result, otherwise it is minimum signed precision.
     Some examples:
     If PREC0 or PREC1 is 8, it means that argument is [0, 0xff],
     if PREC0 or PREC1 is 10, it means that argument is [0, 0x3ff],
     if PREC0 or PREC1 is -8, it means that argument is [-0x80, 0x7f],
     if PREC0 or PREC1 is -10, it means that argument is [-0x200, 0x1ff].
     PREC0  CODE  PREC1  RESULT          PREC2  SIGNED vs. UNSIGNED
       8      +     8    [0, 0x1fe]        9    UNSIGNED
       8      +    10    [0, 0x4fe]       11    UNSIGNED
      -8      +    -8    [-0x100, 0xfe]    9    SIGNED
      -8      +   -10    [-0x280, 0x27e]  11    SIGNED
       8      +    -8    [-0x80, 0x17e]   10    SIGNED
       8      +   -10    [-0x200, 0x2fe]  11    SIGNED
      10      +    -8    [-0x80, 0x47e]   12    SIGNED
       8      -     8    [-0xff, 0xff]     9    SIGNED
       8      -    10    [-0x3ff, 0xff]   11    SIGNED
      10      -     8    [-0xff, 0x3ff]   11    SIGNED
      -8      -    -8    [-0xff, 0xff]     9    SIGNED
      -8      -   -10    [-0x27f, 0x27f]  11    SIGNED
     -10      -    -8    [-0x27f, 0x27f]  11    SIGNED
       8      -    -8    [-0x7f, 0x17f]   10    SIGNED
       8      -   -10    [-0x1ff, 0x2ff]  11    SIGNED
      10      -    -8    [-0x7f, 0x47f]   12    SIGNED
      -8      -     8    [-0x17f, 0x7f]   10    SIGNED
      -8      -    10    [-0x47f, 0x7f]   12    SIGNED
     -10      -     8    [-0x2ff, 0x1ff]  11    SIGNED  */
  int prec2 = MAX (prec0 < 0 ? -prec0 : prec0,
		   prec1 < 0 ? -prec1 : prec1);
	    /* If operands are either both signed or both unsigned,
	       we need just one additional bit.  */
  prec2 = (((prec0 < 0) == (prec1 < 0)
	       /* If one operand is signed and one unsigned and
		  the signed one has larger precision, we need
		  just one extra bit, otherwise two.  */
	    || (prec0 < 0 ? (prec2 == -prec0 && prec2 != prec1)
			  : (prec2 == -prec1 && prec2 != prec0)))
	   ? prec2 + 1 : prec2 + 2);
  int prec3 = MAX (prec0 < 0 ? -prec0 : prec0,
		   prec1 < 0 ? -prec1 : prec1);
  prec3 = MAX (prec3, prec);
  tree var = NULL_TREE;
  tree orig_obj = obj;
  if (obj == NULL_TREE
      && TREE_CODE (type) == BITINT_TYPE
      && bitint_precision_kind (type) >= bitint_prec_large
      && m_names
      && bitmap_bit_p (m_names, SSA_NAME_VERSION (lhs)))
    {
      int part = var_to_partition (m_map, lhs);
      gcc_assert (m_vars[part] != NULL_TREE);
      obj = m_vars[part];
      if (TREE_TYPE (lhs) == type)
	orig_obj = obj;
    }
  if (TREE_CODE (type) != BITINT_TYPE
      || bitint_precision_kind (type) < bitint_prec_large)
    {
      unsigned HOST_WIDE_INT nelts = CEIL (prec, limb_prec);
      tree atype = build_array_type_nelts (m_limb_type, nelts);
      var = create_tmp_var (atype);
    }

  enum tree_code code;
  switch (gimple_call_internal_fn (stmt))
    {
    case IFN_ADD_OVERFLOW:
    case IFN_UBSAN_CHECK_ADD:
      code = PLUS_EXPR;
      break;
    case IFN_SUB_OVERFLOW:
    case IFN_UBSAN_CHECK_SUB:
      code = MINUS_EXPR;
      break;
    default:
      gcc_unreachable ();
    }
  unsigned start, end;
  bool check_zero;
  tree ovf = arith_overflow (code, type, prec, prec0, prec1, prec2,
			     &start, &end, &check_zero);

  unsigned startlimb, endlimb;
  if (ovf)
    {
      startlimb = ~0U;
      endlimb = ~0U;
    }
  else
    {
      startlimb = start / limb_prec;
      endlimb = (end - 1) / limb_prec;
    }

  int prec4 = ovf != NULL_TREE ? prec : prec3;
  bitint_prec_kind kind = bitint_precision_kind (prec4);
  unsigned cnt, rem = 0, fin = 0;
  tree idx = NULL_TREE, idx_first = NULL_TREE, idx_next = NULL_TREE;
  bool last_ovf = (ovf == NULL_TREE
		   && CEIL (prec2, limb_prec) > CEIL (prec3, limb_prec));
  if (kind != bitint_prec_huge)
    cnt = CEIL (prec4, limb_prec) + last_ovf;
  else
    {
      rem = (prec4 % (2 * limb_prec));
      fin = (prec4 - rem) / limb_prec;
      cnt = 2 + CEIL (rem, limb_prec) + last_ovf;
      idx = idx_first = create_loop (size_zero_node, &idx_next);
    }

  if (kind == bitint_prec_huge)
    m_upwards_2limb = fin;
  m_upwards = true;

  tree type0 = TREE_TYPE (arg0);
  tree type1 = TREE_TYPE (arg1);
  int prec5 = prec3;
  if (bitint_precision_kind (prec5) < bitint_prec_large)
    prec5 = MAX (TYPE_PRECISION (type0), TYPE_PRECISION (type1));
  if (TYPE_PRECISION (type0) < prec5)
    {
      type0 = build_bitint_type (prec5, TYPE_UNSIGNED (type0));
      if (TREE_CODE (arg0) == INTEGER_CST)
	arg0 = fold_convert (type0, arg0);
    }
  if (TYPE_PRECISION (type1) < prec5)
    {
      type1 = build_bitint_type (prec5, TYPE_UNSIGNED (type1));
      if (TREE_CODE (arg1) == INTEGER_CST)
	arg1 = fold_convert (type1, arg1);
    }
  unsigned int data_cnt = 0;
  tree last_rhs1 = NULL_TREE, last_rhs2 = NULL_TREE;
  tree cmp = build_zero_cst (m_limb_type);
  unsigned prec_limbs = CEIL ((unsigned) prec, limb_prec);
  tree ovf_out = NULL_TREE, cmp_out = NULL_TREE;
  for (unsigned i = 0; i < cnt; i++)
    {
      m_data_cnt = 0;
      tree rhs1, rhs2;
      if (kind != bitint_prec_huge)
	idx = size_int (i);
      else if (i >= 2)
	idx = size_int (fin + i - 2);
      if (!last_ovf || i < cnt - 1)
	{
	  if (type0 != TREE_TYPE (arg0))
	    rhs1 = handle_cast (type0, arg0, idx);
	  else
	    rhs1 = handle_operand (arg0, idx);
	  if (type1 != TREE_TYPE (arg1))
	    rhs2 = handle_cast (type1, arg1, idx);
	  else
	    rhs2 = handle_operand (arg1, idx);
	  if (i == 0)
	    data_cnt = m_data_cnt;
	  if (!useless_type_conversion_p (m_limb_type, TREE_TYPE (rhs1)))
	    rhs1 = add_cast (m_limb_type, rhs1);
	  if (!useless_type_conversion_p (m_limb_type, TREE_TYPE (rhs2)))
	    rhs2 = add_cast (m_limb_type, rhs2);
	  last_rhs1 = rhs1;
	  last_rhs2 = rhs2;
	}
      else
	{
	  m_data_cnt = data_cnt;
	  if (TYPE_UNSIGNED (type0))
	    rhs1 = build_zero_cst (m_limb_type);
	  else
	    {
	      rhs1 = add_cast (signed_type_for (m_limb_type), last_rhs1);
	      if (TREE_CODE (rhs1) == INTEGER_CST)
		rhs1 = build_int_cst (m_limb_type,
				      tree_int_cst_sgn (rhs1) < 0 ? -1 : 0);
	      else
		{
		  tree lpm1 = build_int_cst (unsigned_type_node,
					     limb_prec - 1);
		  g = gimple_build_assign (make_ssa_name (TREE_TYPE (rhs1)),
					   RSHIFT_EXPR, rhs1, lpm1);
		  insert_before (g);
		  rhs1 = add_cast (m_limb_type, gimple_assign_lhs (g));
		}
	    }
	  if (TYPE_UNSIGNED (type1))
	    rhs2 = build_zero_cst (m_limb_type);
	  else
	    {
	      rhs2 = add_cast (signed_type_for (m_limb_type), last_rhs2);
	      if (TREE_CODE (rhs2) == INTEGER_CST)
		rhs2 = build_int_cst (m_limb_type,
				      tree_int_cst_sgn (rhs2) < 0 ? -1 : 0);
	      else
		{
		  tree lpm1 = build_int_cst (unsigned_type_node,
					     limb_prec - 1);
		  g = gimple_build_assign (make_ssa_name (TREE_TYPE (rhs2)),
					   RSHIFT_EXPR, rhs2, lpm1);
		  insert_before (g);
		  rhs2 = add_cast (m_limb_type, gimple_assign_lhs (g));
		}
	    }
	}
      tree rhs = handle_plus_minus (code, rhs1, rhs2, idx);
      if (ovf != boolean_false_node)
	{
	  if (tree_fits_uhwi_p (idx))
	    {
	      unsigned limb = tree_to_uhwi (idx);
	      if (limb >= startlimb && limb <= endlimb)
		{
		  tree l = arith_overflow_extract_bits (start, end, rhs,
							limb, check_zero);
		  tree this_ovf = make_ssa_name (boolean_type_node);
		  if (ovf == NULL_TREE && !check_zero)
		    {
		      cmp = l;
		      g = gimple_build_assign (make_ssa_name (m_limb_type),
					       PLUS_EXPR, l,
					       build_int_cst (m_limb_type, 1));
		      insert_before (g);
		      g = gimple_build_assign (this_ovf, GT_EXPR,
					       gimple_assign_lhs (g),
					       build_int_cst (m_limb_type, 1));
		    }
		  else
		    g = gimple_build_assign (this_ovf, NE_EXPR, l, cmp);
		  insert_before (g);
		  if (ovf == NULL_TREE)
		    ovf = this_ovf;
		  else
		    {
		      tree b = make_ssa_name (boolean_type_node);
		      g = gimple_build_assign (b, BIT_IOR_EXPR, ovf, this_ovf);
		      insert_before (g);
		      ovf = b;
		    }
		}
	    }
	  else if (startlimb < fin)
	    {
	      if (m_first && startlimb + 2 < fin)
		{
		  tree data_out;
		  ovf = prepare_data_in_out (boolean_false_node, idx, &data_out);
		  ovf_out = m_data.pop ();
		  m_data.pop ();
		  if (!check_zero)
		    {
		      cmp = prepare_data_in_out (cmp, idx, &data_out);
		      cmp_out = m_data.pop ();
		      m_data.pop ();
		    }
		}
	      if (i != 0 || startlimb != fin - 1)
		{
		  tree_code cmp_code;
		  bool single_comparison
		    = (startlimb + 2 >= fin || (startlimb & 1) != (i & 1));
		  if (!single_comparison)
		    cmp_code = GE_EXPR;
		  else if ((startlimb & 1) == (i & 1))
		    cmp_code = EQ_EXPR;
		  else
		    cmp_code = GT_EXPR;
		  g = gimple_build_cond (cmp_code, idx, size_int (startlimb),
					 NULL_TREE, NULL_TREE);
		  edge edge_true_true, edge_true_false, edge_false;
		  gimple *g2 = NULL;
		  if (!single_comparison)
		    g2 = gimple_build_cond (NE_EXPR, idx,
					    size_int (startlimb), NULL_TREE,
					    NULL_TREE);
		  if_then_if_then_else (g, g2, profile_probability::likely (),
					profile_probability::likely (),
					edge_true_true, edge_true_false,
					edge_false);
		  unsigned tidx = startlimb + (cmp_code == GT_EXPR);
		  tree l = arith_overflow_extract_bits (start, end, rhs, tidx,
							check_zero);
		  tree this_ovf = make_ssa_name (boolean_type_node);
		  if (cmp_code != GT_EXPR && !check_zero)
		    {
		      g = gimple_build_assign (make_ssa_name (m_limb_type),
					       PLUS_EXPR, l,
					       build_int_cst (m_limb_type, 1));
		      insert_before (g);
		      g = gimple_build_assign (this_ovf, GT_EXPR,
					       gimple_assign_lhs (g),
					       build_int_cst (m_limb_type, 1));
		    }
		  else
		    g = gimple_build_assign (this_ovf, NE_EXPR, l, cmp);
		  insert_before (g);
		  if (cmp_code == GT_EXPR)
		    {
		      tree t = make_ssa_name (boolean_type_node);
		      g = gimple_build_assign (t, BIT_IOR_EXPR, ovf, this_ovf);
		      insert_before (g);
		      this_ovf = t;
		    }
		  tree this_ovf2 = NULL_TREE;
		  if (!single_comparison)
		    {
		      m_gsi = gsi_after_labels (edge_true_true->src);
		      tree t = make_ssa_name (boolean_type_node);
		      g = gimple_build_assign (t, NE_EXPR, rhs, cmp);
		      insert_before (g);
		      this_ovf2 = make_ssa_name (boolean_type_node);
		      g = gimple_build_assign (this_ovf2, BIT_IOR_EXPR,
					       ovf, t);
		      insert_before (g);
		    }
		  m_gsi = gsi_after_labels (edge_true_false->dest);
		  tree t;
		  if (i == 1 && ovf_out)
		    t = ovf_out;
		  else
		    t = make_ssa_name (boolean_type_node);
		  gphi *phi = create_phi_node (t, edge_true_false->dest);
		  add_phi_arg (phi, this_ovf, edge_true_false,
			       UNKNOWN_LOCATION);
		  add_phi_arg (phi, ovf ? ovf
				    : boolean_false_node, edge_false,
			       UNKNOWN_LOCATION);
		  if (edge_true_true)
		    add_phi_arg (phi, this_ovf2, edge_true_true,
				 UNKNOWN_LOCATION);
		  ovf = t;
		  if (!check_zero && cmp_code != GT_EXPR)
		    {
		      t = cmp_out ? cmp_out : make_ssa_name (m_limb_type);
		      phi = create_phi_node (t, edge_true_false->dest);
		      add_phi_arg (phi, l, edge_true_false, UNKNOWN_LOCATION);
		      add_phi_arg (phi, cmp, edge_false, UNKNOWN_LOCATION);
		      if (edge_true_true)
			add_phi_arg (phi, cmp, edge_true_true,
				     UNKNOWN_LOCATION);
		      cmp = t;
		    }
		}
	    }
	}

      if (var || obj)
	{
	  if (tree_fits_uhwi_p (idx) && tree_to_uhwi (idx) >= prec_limbs)
	    ;
	  else if (!tree_fits_uhwi_p (idx)
		   && (unsigned) prec < (fin - (i == 0)) * limb_prec)
	    {
	      bool single_comparison
		= (((unsigned) prec % limb_prec) == 0
		   || prec_limbs + 1 >= fin
		   || (prec_limbs & 1) == (i & 1));
	      g = gimple_build_cond (LE_EXPR, idx, size_int (prec_limbs - 1),
				     NULL_TREE, NULL_TREE);
	      gimple *g2 = NULL;
	      if (!single_comparison)
		g2 = gimple_build_cond (EQ_EXPR, idx,
					size_int (prec_limbs - 1),
					NULL_TREE, NULL_TREE);
	      edge edge_true_true, edge_true_false, edge_false;
	      if_then_if_then_else (g, g2, profile_probability::likely (),
				    profile_probability::unlikely (),
				    edge_true_true, edge_true_false,
				    edge_false);
	      tree l = limb_access (type, var ? var : obj, idx, true);
	      g = gimple_build_assign (l, rhs);
	      insert_before (g);
	      if (!single_comparison)
		{
		  m_gsi = gsi_after_labels (edge_true_true->src);
		  tree plm1idx = size_int (prec_limbs - 1);
		  tree plm1type = limb_access_type (type, plm1idx);
		  l = limb_access (type, var ? var : obj, plm1idx, true);
		  if (!useless_type_conversion_p (plm1type, TREE_TYPE (rhs)))
		    rhs = add_cast (plm1type, rhs);
		  if (!useless_type_conversion_p (TREE_TYPE (l),
						  TREE_TYPE (rhs)))
		    rhs = add_cast (TREE_TYPE (l), rhs);
		  g = gimple_build_assign (l, rhs);
		  insert_before (g);
		}
	      m_gsi = gsi_after_labels (edge_true_false->dest);
	    }
	  else
	    {
	      tree l = limb_access (type, var ? var : obj, idx, true);
	      if (!useless_type_conversion_p (TREE_TYPE (l), TREE_TYPE (rhs)))
		rhs = add_cast (TREE_TYPE (l), rhs);
	      g = gimple_build_assign (l, rhs);
	      insert_before (g);
	    }
	}
      m_first = false;
      if (kind == bitint_prec_huge && i <= 1)
	{
	  if (i == 0)
	    {
	      idx = make_ssa_name (sizetype);
	      g = gimple_build_assign (idx, PLUS_EXPR, idx_first,
				       size_one_node);
	      insert_before (g);
	    }
	  else
	    {
	      g = gimple_build_assign (idx_next, PLUS_EXPR, idx_first,
				       size_int (2));
	      insert_before (g);
	      g = gimple_build_cond (NE_EXPR, idx_next, size_int (fin),
				     NULL_TREE, NULL_TREE);
	      insert_before (g);
	      m_gsi = gsi_for_stmt (final_stmt);
	      m_bb = NULL;
	    }
	}
    }

  finish_arith_overflow (var, obj, type, ovf, lhs, orig_obj, stmt, code);
}

/* Lower a .MUL_OVERFLOW call with at least one large/huge _BitInt
   argument or return type _Complex large/huge _BitInt.  */

void
bitint_large_huge::lower_mul_overflow (tree obj, gimple *stmt)
{
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      gsi_remove (&gsi, true);
      return;
    }
  gimple *final_stmt = gsi_stmt (m_gsi);
  tree type = TREE_TYPE (lhs);
  if (TREE_CODE (type) == COMPLEX_TYPE)
    type = TREE_TYPE (type);
  int prec = TYPE_PRECISION (type), prec0, prec1;
  arg0 = handle_operand_addr (arg0, stmt, NULL, &prec0);
  arg1 = handle_operand_addr (arg1, stmt, NULL, &prec1);
  int prec2 = ((prec0 < 0 ? -prec0 : prec0)
	       + (prec1 < 0 ? -prec1 : prec1));
  if (prec0 == 1 || prec1 == 1)
    --prec2;
  tree var = NULL_TREE;
  tree orig_obj = obj;
  bool force_var = false;
  if (obj == NULL_TREE
      && TREE_CODE (type) == BITINT_TYPE
      && bitint_precision_kind (type) >= bitint_prec_large
      && m_names
      && bitmap_bit_p (m_names, SSA_NAME_VERSION (lhs)))
    {
      int part = var_to_partition (m_map, lhs);
      gcc_assert (m_vars[part] != NULL_TREE);
      obj = m_vars[part];
      if (TREE_TYPE (lhs) == type)
	orig_obj = obj;
    }
  else if (obj != NULL_TREE && DECL_P (obj))
    {
      for (int i = 0; i < 2; ++i)
	{
	  tree arg = i ? arg1 : arg0;
	  if (TREE_CODE (arg) == ADDR_EXPR)
	    arg = TREE_OPERAND (arg, 0);
	  if (get_base_address (arg) == obj)
	    {
	      force_var = true;
	      break;
	    }
	}
    }
  if (obj == NULL_TREE
      || force_var
      || TREE_CODE (type) != BITINT_TYPE
      || bitint_precision_kind (type) < bitint_prec_large
      || prec2 > (CEIL (prec, limb_prec) * limb_prec * (orig_obj ? 1 : 2)))
    {
      unsigned HOST_WIDE_INT nelts = CEIL (MAX (prec, prec2), limb_prec);
      tree atype = build_array_type_nelts (m_limb_type, nelts);
      var = create_tmp_var (atype);
    }
  tree addr = build_fold_addr_expr (var ? var : obj);
  addr = force_gimple_operand_gsi (&m_gsi, addr, true,
				   NULL_TREE, true, GSI_SAME_STMT);
  tree sitype = lang_hooks.types.type_for_mode (SImode, 0);
  gimple *g
    = gimple_build_call_internal (IFN_MULBITINT, 6,
				  addr, build_int_cst (sitype,
						       MAX (prec2, prec)),
				  arg0, build_int_cst (sitype, prec0),
				  arg1, build_int_cst (sitype, prec1));
  insert_before (g);

  unsigned start, end;
  bool check_zero;
  tree ovf = arith_overflow (MULT_EXPR, type, prec, prec0, prec1, prec2,
			     &start, &end, &check_zero);
  if (ovf == NULL_TREE)
    {
      unsigned startlimb = start / limb_prec;
      unsigned endlimb = (end - 1) / limb_prec;
      unsigned cnt;
      bool use_loop = false;
      if (startlimb == endlimb)
	cnt = 1;
      else if (startlimb + 1 == endlimb)
	cnt = 2;
      else if ((end % limb_prec) == 0)
	{
	  cnt = 2;
	  use_loop = true;
	}
      else
	{
	  cnt = 3;
	  use_loop = startlimb + 2 < endlimb;
	}
      if (cnt == 1)
	{
	  tree l = limb_access (NULL_TREE, var ? var : obj,
				size_int (startlimb), true);
	  g = gimple_build_assign (make_ssa_name (m_limb_type), l);
	  insert_before (g);
	  l = arith_overflow_extract_bits (start, end, gimple_assign_lhs (g),
					   startlimb, check_zero);
	  ovf = make_ssa_name (boolean_type_node);
	  if (check_zero)
	    g = gimple_build_assign (ovf, NE_EXPR, l,
				     build_zero_cst (m_limb_type));
	  else
	    {
	      g = gimple_build_assign (make_ssa_name (m_limb_type),
				       PLUS_EXPR, l,
				       build_int_cst (m_limb_type, 1));
	      insert_before (g);
	      g = gimple_build_assign (ovf, GT_EXPR, gimple_assign_lhs (g),
				       build_int_cst (m_limb_type, 1));
	    }
	  insert_before (g);
	}
      else
	{
	  basic_block edge_bb = NULL;
	  gimple_stmt_iterator gsi = m_gsi;
	  gsi_prev (&gsi);
	  edge e = split_block (gsi_bb (gsi), gsi_stmt (gsi));
	  edge_bb = e->src;
	  m_gsi = gsi_end_bb (edge_bb);

	  tree cmp = build_zero_cst (m_limb_type);
	  for (unsigned i = 0; i < cnt; i++)
	    {
	      tree idx, idx_next = NULL_TREE;
	      if (i == 0)
		idx = size_int (startlimb);
	      else if (i == 2)
		idx = size_int (endlimb);
	      else if (use_loop)
		idx = create_loop (size_int (startlimb + 1), &idx_next);
	      else
		idx = size_int (startlimb + 1);
	      tree l = limb_access (NULL_TREE, var ? var : obj, idx, true);
	      g = gimple_build_assign (make_ssa_name (m_limb_type), l);
	      insert_before (g);
	      l = gimple_assign_lhs (g);
	      if (i == 0 || i == 2)
		l = arith_overflow_extract_bits (start, end, l,
						 tree_to_uhwi (idx),
						 check_zero);
	      if (i == 0 && !check_zero)
		{
		  cmp = l;
		  g = gimple_build_assign (make_ssa_name (m_limb_type),
					   PLUS_EXPR, l,
					   build_int_cst (m_limb_type, 1));
		  insert_before (g);
		  g = gimple_build_cond (GT_EXPR, gimple_assign_lhs (g),
					 build_int_cst (m_limb_type, 1),
					 NULL_TREE, NULL_TREE);
		}
	      else
		g = gimple_build_cond (NE_EXPR, l, cmp, NULL_TREE, NULL_TREE);
	      insert_before (g);
	      edge e1 = split_block (gsi_bb (m_gsi), g);
	      e1->flags = EDGE_FALSE_VALUE;
	      edge e2 = make_edge (e1->src, gimple_bb (final_stmt),
				   EDGE_TRUE_VALUE);
	      e1->probability = profile_probability::likely ();
	      e2->probability = e1->probability.invert ();
	      if (i == 0)
		set_immediate_dominator (CDI_DOMINATORS, e2->dest, e2->src);
	      m_gsi = gsi_after_labels (e1->dest);
	      if (i == 1 && use_loop)
		{
		  g = gimple_build_assign (idx_next, PLUS_EXPR, idx,
					   size_one_node);
		  insert_before (g);
		  g = gimple_build_cond (NE_EXPR, idx_next,
					 size_int (endlimb + (cnt == 2)),
					 NULL_TREE, NULL_TREE);
		  insert_before (g);
		  edge true_edge, false_edge;
		  extract_true_false_edges_from_block (gsi_bb (m_gsi),
						       &true_edge,
						       &false_edge);
		  m_gsi = gsi_after_labels (false_edge->dest);
		  m_bb = NULL;
		}
	    }

	  ovf = make_ssa_name (boolean_type_node);
	  basic_block bb = gimple_bb (final_stmt);
	  gphi *phi = create_phi_node (ovf, bb);
	  edge e1 = find_edge (gsi_bb (m_gsi), bb);
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      tree val = e == e1 ? boolean_false_node : boolean_true_node;
	      add_phi_arg (phi, val, e, UNKNOWN_LOCATION);
	    }
	  m_gsi = gsi_for_stmt (final_stmt);
	}
    }

  finish_arith_overflow (var, obj, type, ovf, lhs, orig_obj, stmt, MULT_EXPR);
}

/* Lower REALPART_EXPR or IMAGPART_EXPR stmt extracting part of result from
   .{ADD,SUB,MUL}_OVERFLOW call.  */

void
bitint_large_huge::lower_cplxpart_stmt (tree obj, gimple *stmt)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  rhs1 = TREE_OPERAND (rhs1, 0);
  if (obj == NULL_TREE)
    {
      int part = var_to_partition (m_map, gimple_assign_lhs (stmt));
      gcc_assert (m_vars[part] != NULL_TREE);
      obj = m_vars[part];
    }
  if (TREE_CODE (rhs1) == SSA_NAME
      && (m_names == NULL
	  || !bitmap_bit_p (m_names, SSA_NAME_VERSION (rhs1))))
    {
      lower_call (obj, SSA_NAME_DEF_STMT (rhs1));
      return;
    }
  int part = var_to_partition (m_map, rhs1);
  gcc_assert (m_vars[part] != NULL_TREE);
  tree var = m_vars[part];
  unsigned HOST_WIDE_INT nelts
    = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (obj))) / limb_prec;
  tree atype = build_array_type_nelts (m_limb_type, nelts);
  if (!useless_type_conversion_p (atype, TREE_TYPE (obj)))
    obj = build1 (VIEW_CONVERT_EXPR, atype, obj);
  tree off = build_int_cst (build_pointer_type (TREE_TYPE (var)),
			    gimple_assign_rhs_code (stmt) == REALPART_EXPR
			    ? 0 : nelts * m_limb_size);
  tree v2 = build2 (MEM_REF, atype, build_fold_addr_expr (var), off);
  gimple *g = gimple_build_assign (obj, v2);
  insert_before (g);
}

/* Lower COMPLEX_EXPR stmt.  */

void
bitint_large_huge::lower_complexexpr_stmt (gimple *stmt)
{
  tree lhs = gimple_assign_lhs (stmt);
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  int part = var_to_partition (m_map, lhs);
  gcc_assert (m_vars[part] != NULL_TREE);
  lhs = m_vars[part];
  unsigned HOST_WIDE_INT nelts
    = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (rhs1))) / limb_prec;
  tree atype = build_array_type_nelts (m_limb_type, nelts);
  tree zero = build_zero_cst (build_pointer_type (TREE_TYPE (lhs)));
  tree v1 = build2 (MEM_REF, atype, build_fold_addr_expr (lhs), zero);
  tree v2;
  if (TREE_CODE (rhs1) == SSA_NAME)
    {
      part = var_to_partition (m_map, rhs1);
      gcc_assert (m_vars[part] != NULL_TREE);
      v2 = m_vars[part];
    }
  else if (integer_zerop (rhs1))
    v2 = build_zero_cst (atype);
  else
    v2 = tree_output_constant_def (rhs1);
  if (!useless_type_conversion_p (atype, TREE_TYPE (v2)))
    v2 = build1 (VIEW_CONVERT_EXPR, atype, v2);
  gimple *g = gimple_build_assign (v1, v2);
  insert_before (g);
  tree off = fold_convert (build_pointer_type (TREE_TYPE (lhs)),
			   TYPE_SIZE_UNIT (atype));
  v1 = build2 (MEM_REF, atype, build_fold_addr_expr (lhs), off);
  if (TREE_CODE (rhs2) == SSA_NAME)
    {
      part = var_to_partition (m_map, rhs2);
      gcc_assert (m_vars[part] != NULL_TREE);
      v2 = m_vars[part];
    }
  else if (integer_zerop (rhs2))
    v2 = build_zero_cst (atype);
  else
    v2 = tree_output_constant_def (rhs2);
  if (!useless_type_conversion_p (atype, TREE_TYPE (v2)))
    v2 = build1 (VIEW_CONVERT_EXPR, atype, v2);
  g = gimple_build_assign (v1, v2);
  insert_before (g);
}

/* Lower a .{CLZ,CTZ,CLRSB,FFS,PARITY,POPCOUNT} call with one large/huge _BitInt
   argument.  */

void
bitint_large_huge::lower_bit_query (gimple *stmt)
{
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = (gimple_call_num_args (stmt) == 2
	       ? gimple_call_arg (stmt, 1) : NULL_TREE);
  tree lhs = gimple_call_lhs (stmt);
  gimple *g;

  if (!lhs)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      gsi_remove (&gsi, true);
      return;
    }
  tree type = TREE_TYPE (arg0);
  gcc_assert (TREE_CODE (type) == BITINT_TYPE);
  bitint_prec_kind kind = bitint_precision_kind (type);
  gcc_assert (kind >= bitint_prec_large);
  enum internal_fn ifn = gimple_call_internal_fn (stmt);
  enum built_in_function fcode = END_BUILTINS;
  gcc_assert (TYPE_PRECISION (unsigned_type_node) == limb_prec
	      || TYPE_PRECISION (long_unsigned_type_node) == limb_prec
	      || TYPE_PRECISION (long_long_unsigned_type_node) == limb_prec);
  switch (ifn)
    {
    case IFN_CLZ:
      if (TYPE_PRECISION (unsigned_type_node) == limb_prec)
	fcode = BUILT_IN_CLZ;
      else if (TYPE_PRECISION (long_unsigned_type_node) == limb_prec)
	fcode = BUILT_IN_CLZL;
      else
	fcode = BUILT_IN_CLZLL;
      break;
    case IFN_FFS:
      /* .FFS (X) is .CTZ (X, -1) + 1, though under the hood
	 we don't add the addend at the end.  */
      arg1 = integer_zero_node;
      /* FALLTHRU */
    case IFN_CTZ:
      if (TYPE_PRECISION (unsigned_type_node) == limb_prec)
	fcode = BUILT_IN_CTZ;
      else if (TYPE_PRECISION (long_unsigned_type_node) == limb_prec)
	fcode = BUILT_IN_CTZL;
      else
	fcode = BUILT_IN_CTZLL;
      m_upwards = true;
      break;
    case IFN_CLRSB:
      if (TYPE_PRECISION (unsigned_type_node) == limb_prec)
	fcode = BUILT_IN_CLRSB;
      else if (TYPE_PRECISION (long_unsigned_type_node) == limb_prec)
	fcode = BUILT_IN_CLRSBL;
      else
	fcode = BUILT_IN_CLRSBLL;
      break;
    case IFN_PARITY:
      if (TYPE_PRECISION (unsigned_type_node) == limb_prec)
	fcode = BUILT_IN_PARITY;
      else if (TYPE_PRECISION (long_unsigned_type_node) == limb_prec)
	fcode = BUILT_IN_PARITYL;
      else
	fcode = BUILT_IN_PARITYLL;
      m_upwards = true;
      break;
    case IFN_POPCOUNT:
      if (TYPE_PRECISION (unsigned_type_node) == limb_prec)
	fcode = BUILT_IN_POPCOUNT;
      else if (TYPE_PRECISION (long_unsigned_type_node) == limb_prec)
	fcode = BUILT_IN_POPCOUNTL;
      else
	fcode = BUILT_IN_POPCOUNTLL;
      m_upwards = true;
      break;
    default:
      gcc_unreachable ();
    }
  tree fndecl = builtin_decl_explicit (fcode), res = NULL_TREE;
  unsigned cnt = 0, rem = 0, end = 0, prec = TYPE_PRECISION (type);
  struct bq_details { edge e; tree val, addend; } *bqp = NULL;
  basic_block edge_bb = NULL;
  if (m_upwards)
    {
      tree idx = NULL_TREE, idx_first = NULL_TREE, idx_next = NULL_TREE;
      if (kind == bitint_prec_large)
	cnt = CEIL (prec, limb_prec);
      else
	{
	  rem = (prec % (2 * limb_prec));
	  end = (prec - rem) / limb_prec;
	  cnt = 2 + CEIL (rem, limb_prec);
	  idx = idx_first = create_loop (size_zero_node, &idx_next);
	}

      if (ifn == IFN_CTZ || ifn == IFN_FFS)
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	  gsi_prev (&gsi);
	  edge e = split_block (gsi_bb (gsi), gsi_stmt (gsi));
	  edge_bb = e->src;
	  if (kind == bitint_prec_large)
	    m_gsi = gsi_end_bb (edge_bb);
	  bqp = XALLOCAVEC (struct bq_details, cnt);
	}
      else
	m_after_stmt = stmt;
      if (kind != bitint_prec_large)
	m_upwards_2limb = end;

      for (unsigned i = 0; i < cnt; i++)
	{
	  m_data_cnt = 0;
	  if (kind == bitint_prec_large)
	    idx = size_int (i);
	  else if (i >= 2)
	    idx = size_int (end + (i > 2));

	  tree rhs1 = handle_operand (arg0, idx);
	  if (!useless_type_conversion_p (m_limb_type, TREE_TYPE (rhs1)))
	    {
	      if (!TYPE_UNSIGNED (TREE_TYPE (rhs1)))
		rhs1 = add_cast (unsigned_type_for (TREE_TYPE (rhs1)), rhs1);
	      rhs1 = add_cast (m_limb_type, rhs1);
	    }

	  tree in, out, tem;
	  if (ifn == IFN_PARITY)
	    in = prepare_data_in_out (build_zero_cst (m_limb_type), idx, &out);
	  else if (ifn == IFN_FFS)
	    in = prepare_data_in_out (integer_one_node, idx, &out);
	  else
	    in = prepare_data_in_out (integer_zero_node, idx, &out);

	  switch (ifn)
	    {
	    case IFN_CTZ:
	    case IFN_FFS:
	      g = gimple_build_cond (NE_EXPR, rhs1,
				     build_zero_cst (m_limb_type),
				     NULL_TREE, NULL_TREE);
	      insert_before (g);
	      edge e1, e2;
	      e1 = split_block (gsi_bb (m_gsi), g);
	      e1->flags = EDGE_FALSE_VALUE;
	      e2 = make_edge (e1->src, gimple_bb (stmt), EDGE_TRUE_VALUE);
	      e1->probability = profile_probability::unlikely ();
	      e2->probability = e1->probability.invert ();
	      if (i == 0)
		set_immediate_dominator (CDI_DOMINATORS, e2->dest, e2->src);
	      m_gsi = gsi_after_labels (e1->dest);
	      bqp[i].e = e2;
	      bqp[i].val = rhs1;
	      if (tree_fits_uhwi_p (idx))
		bqp[i].addend
		  = build_int_cst (integer_type_node,
				   tree_to_uhwi (idx) * limb_prec
				   + (ifn == IFN_FFS));
	      else
		{
		  bqp[i].addend = in;
		  if (i == 1)
		    res = out;
		  else
		    res = make_ssa_name (integer_type_node);
		  g = gimple_build_assign (res, PLUS_EXPR, in,
					   build_int_cst (integer_type_node,
							  limb_prec));
		  insert_before (g);
		  m_data[m_data_cnt] = res;
		}
	      break;
	    case IFN_PARITY:
	      if (!integer_zerop (in))
		{
		  if (kind == bitint_prec_huge && i == 1)
		    res = out;
		  else
		    res = make_ssa_name (m_limb_type);
		  g = gimple_build_assign (res, BIT_XOR_EXPR, in, rhs1);
		  insert_before (g);
		}
	      else
		res = rhs1;
	      m_data[m_data_cnt] = res;
	      break;
	    case IFN_POPCOUNT:
	      g = gimple_build_call (fndecl, 1, rhs1);
	      tem = make_ssa_name (integer_type_node);
	      gimple_call_set_lhs (g, tem);
	      insert_before (g);
	      if (!integer_zerop (in))
		{
		  if (kind == bitint_prec_huge && i == 1)
		    res = out;
		  else
		    res = make_ssa_name (integer_type_node);
		  g = gimple_build_assign (res, PLUS_EXPR, in, tem);
		  insert_before (g);
		}
	      else
		res = tem;
	      m_data[m_data_cnt] = res;
	      break;
	    default:
	      gcc_unreachable ();
	    }

	  m_first = false;
	  if (kind == bitint_prec_huge && i <= 1)
	    {
	      if (i == 0)
		{
		  idx = make_ssa_name (sizetype);
		  g = gimple_build_assign (idx, PLUS_EXPR, idx_first,
					   size_one_node);
		  insert_before (g);
		}
	      else
		{
		  g = gimple_build_assign (idx_next, PLUS_EXPR, idx_first,
					   size_int (2));
		  insert_before (g);
		  g = gimple_build_cond (NE_EXPR, idx_next, size_int (end),
					 NULL_TREE, NULL_TREE);
		  insert_before (g);
		  if (ifn == IFN_CTZ || ifn == IFN_FFS)
		    m_gsi = gsi_after_labels (edge_bb);
		  else
		    m_gsi = gsi_for_stmt (stmt);
		  m_bb = NULL;
		}
	    }
	}
    }
  else
    {
      tree idx = NULL_TREE, idx_next = NULL_TREE, first = NULL_TREE;
      int sub_one = 0;
      if (kind == bitint_prec_large)
	cnt = CEIL (prec, limb_prec);
      else
	{
	  rem = prec % limb_prec;
	  if (rem == 0 && (!TYPE_UNSIGNED (type) || ifn == IFN_CLRSB))
	    rem = limb_prec;
	  end = (prec - rem) / limb_prec;
	  cnt = 1 + (rem != 0);
	  if (ifn == IFN_CLRSB)
	    sub_one = 1;
	}

      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      gsi_prev (&gsi);
      edge e = split_block (gsi_bb (gsi), gsi_stmt (gsi));
      edge_bb = e->src;
      m_gsi = gsi_end_bb (edge_bb);

      if (ifn == IFN_CLZ)
	bqp = XALLOCAVEC (struct bq_details, cnt);
      else
	{
	  gsi = gsi_for_stmt (stmt);
	  gsi_prev (&gsi);
	  e = split_block (gsi_bb (gsi), gsi_stmt (gsi));
	  edge_bb = e->src;
	  bqp = XALLOCAVEC (struct bq_details, 2 * cnt);
	}

      for (unsigned i = 0; i < cnt; i++)
	{
	  m_data_cnt = 0;
	  if (kind == bitint_prec_large)
	    idx = size_int (cnt - i - 1);
	  else if (i == cnt - 1)
	    idx = create_loop (size_int (end - 1), &idx_next);
	  else
	    idx = size_int (end);

	  tree rhs1 = handle_operand (arg0, idx);
	  if (!useless_type_conversion_p (m_limb_type, TREE_TYPE (rhs1)))
	    {
	      if (ifn == IFN_CLZ && !TYPE_UNSIGNED (TREE_TYPE (rhs1)))
		rhs1 = add_cast (unsigned_type_for (TREE_TYPE (rhs1)), rhs1);
	      else if (ifn == IFN_CLRSB && TYPE_UNSIGNED (TREE_TYPE (rhs1)))
		rhs1 = add_cast (signed_type_for (TREE_TYPE (rhs1)), rhs1);
	      rhs1 = add_cast (m_limb_type, rhs1);
	    }

	  if (ifn == IFN_CLZ)
	    {
	      g = gimple_build_cond (NE_EXPR, rhs1,
				     build_zero_cst (m_limb_type),
				     NULL_TREE, NULL_TREE);
	      insert_before (g);
	      edge e1 = split_block (gsi_bb (m_gsi), g);
	      e1->flags = EDGE_FALSE_VALUE;
	      edge e2 = make_edge (e1->src, gimple_bb (stmt), EDGE_TRUE_VALUE);
	      e1->probability = profile_probability::unlikely ();
	      e2->probability = e1->probability.invert ();
	      if (i == 0)
		set_immediate_dominator (CDI_DOMINATORS, e2->dest, e2->src);
	      m_gsi = gsi_after_labels (e1->dest);
	      bqp[i].e = e2;
	      bqp[i].val = rhs1;
	    }
	  else
	    {
	      if (i == 0)
		{
		  first = rhs1;
		  g = gimple_build_assign (make_ssa_name (m_limb_type),
					   PLUS_EXPR, rhs1,
					   build_int_cst (m_limb_type, 1));
		  insert_before (g);
		  g = gimple_build_cond (GT_EXPR, gimple_assign_lhs (g),
					 build_int_cst (m_limb_type, 1),
					 NULL_TREE, NULL_TREE);
		  insert_before (g);
		}
	      else
		{
		  g = gimple_build_assign (make_ssa_name (m_limb_type),
					   BIT_XOR_EXPR, rhs1, first);
		  insert_before (g);
		  tree stype = signed_type_for (m_limb_type);
		  g = gimple_build_cond (LT_EXPR,
					 add_cast (stype,
						   gimple_assign_lhs (g)),
					 build_zero_cst (stype),
					 NULL_TREE, NULL_TREE);
		  insert_before (g);
		  edge e1 = split_block (gsi_bb (m_gsi), g);
		  e1->flags = EDGE_FALSE_VALUE;
		  edge e2 = make_edge (e1->src, gimple_bb (stmt),
				       EDGE_TRUE_VALUE);
		  e1->probability = profile_probability::unlikely ();
		  e2->probability = e1->probability.invert ();
		  if (i == 1)
		    set_immediate_dominator (CDI_DOMINATORS, e2->dest,
					     e2->src);
		  m_gsi = gsi_after_labels (e1->dest);
		  bqp[2 * i].e = e2;
		  g = gimple_build_cond (NE_EXPR, rhs1, first,
					 NULL_TREE, NULL_TREE);
		  insert_before (g);
		}
	      edge e1 = split_block (gsi_bb (m_gsi), g);
	      e1->flags = EDGE_FALSE_VALUE;
	      edge e2 = make_edge (e1->src, edge_bb, EDGE_TRUE_VALUE);
	      e1->probability = profile_probability::unlikely ();
	      e2->probability = e1->probability.invert ();
	      if (i == 0)
		set_immediate_dominator (CDI_DOMINATORS, e2->dest, e2->src);
	      m_gsi = gsi_after_labels (e1->dest);
	      bqp[2 * i + 1].e = e2;
	      bqp[i].val = rhs1;
	    }
	  if (tree_fits_uhwi_p (idx))
	    bqp[i].addend
	      = build_int_cst (integer_type_node,
			       (int) prec
			       - (((int) tree_to_uhwi (idx) + 1)
				  * limb_prec) - sub_one);
	  else
	    {
	      tree in, out;
	      in = build_int_cst (integer_type_node, rem - sub_one);
	      m_first = true;
	      in = prepare_data_in_out (in, idx, &out);
	      out = m_data[m_data_cnt + 1];
	      bqp[i].addend = in;
	      g = gimple_build_assign (out, PLUS_EXPR, in,
				       build_int_cst (integer_type_node,
						      limb_prec));
	      insert_before (g);
	      m_data[m_data_cnt] = out;
	    }

	  m_first = false;
	  if (kind == bitint_prec_huge && i == cnt - 1)
	    {
	      g = gimple_build_assign (idx_next, PLUS_EXPR, idx,
				       size_int (-1));
	      insert_before (g);
	      g = gimple_build_cond (NE_EXPR, idx, size_zero_node,
				     NULL_TREE, NULL_TREE);
	      insert_before (g);
	      edge true_edge, false_edge;
	      extract_true_false_edges_from_block (gsi_bb (m_gsi),
						   &true_edge, &false_edge);
	      m_gsi = gsi_after_labels (false_edge->dest);
	      m_bb = NULL;
	    }
	}
    }
  switch (ifn)
    {
    case IFN_CLZ:
    case IFN_CTZ:
    case IFN_FFS:
      gphi *phi1, *phi2, *phi3;
      basic_block bb;
      bb = gsi_bb (m_gsi);
      remove_edge (find_edge (bb, gimple_bb (stmt)));
      phi1 = create_phi_node (make_ssa_name (m_limb_type),
			      gimple_bb (stmt));
      phi2 = create_phi_node (make_ssa_name (integer_type_node),
			      gimple_bb (stmt));
      for (unsigned i = 0; i < cnt; i++)
	{
	  add_phi_arg (phi1, bqp[i].val, bqp[i].e, UNKNOWN_LOCATION);
	  add_phi_arg (phi2, bqp[i].addend, bqp[i].e, UNKNOWN_LOCATION);
	}
      if (arg1 == NULL_TREE)
	{
	  g = gimple_build_builtin_unreachable (m_loc);
	  insert_before (g);
	}
      m_gsi = gsi_for_stmt (stmt);
      g = gimple_build_call (fndecl, 1, gimple_phi_result (phi1));
      gimple_call_set_lhs (g, make_ssa_name (integer_type_node));
      insert_before (g);
      if (arg1 == NULL_TREE)
	g = gimple_build_assign (lhs, PLUS_EXPR,
				 gimple_phi_result (phi2),
				 gimple_call_lhs (g));
      else
	{
	  g = gimple_build_assign (make_ssa_name (integer_type_node),
				   PLUS_EXPR, gimple_phi_result (phi2),
				   gimple_call_lhs (g));
	  insert_before (g);
	  edge e1 = split_block (gimple_bb (stmt), g);
	  edge e2 = make_edge (bb, e1->dest, EDGE_FALLTHRU);
	  e2->probability = profile_probability::always ();
	  set_immediate_dominator (CDI_DOMINATORS, e1->dest,
				   get_immediate_dominator (CDI_DOMINATORS,
							    e1->src));
	  phi3 = create_phi_node (make_ssa_name (integer_type_node), e1->dest);
	  add_phi_arg (phi3, gimple_assign_lhs (g), e1, UNKNOWN_LOCATION);
	  add_phi_arg (phi3, arg1, e2, UNKNOWN_LOCATION);
	  m_gsi = gsi_for_stmt (stmt);
	  g = gimple_build_assign (lhs, gimple_phi_result (phi3));
	}
      gsi_replace (&m_gsi, g, true);
      break;
    case IFN_CLRSB:
      bb = gsi_bb (m_gsi);
      remove_edge (find_edge (bb, edge_bb));
      edge e;
      e = make_edge (bb, gimple_bb (stmt), EDGE_FALLTHRU);
      e->probability = profile_probability::always ();
      set_immediate_dominator (CDI_DOMINATORS, gimple_bb (stmt),
			       get_immediate_dominator (CDI_DOMINATORS,
							edge_bb));
      phi1 = create_phi_node (make_ssa_name (m_limb_type),
			      edge_bb);
      phi2 = create_phi_node (make_ssa_name (integer_type_node),
			      edge_bb);
      phi3 = create_phi_node (make_ssa_name (integer_type_node),
			      gimple_bb (stmt));
      for (unsigned i = 0; i < cnt; i++)
	{
	  add_phi_arg (phi1, bqp[i].val, bqp[2 * i + 1].e, UNKNOWN_LOCATION);
	  add_phi_arg (phi2, bqp[i].addend, bqp[2 * i + 1].e,
		       UNKNOWN_LOCATION);
	  tree a = bqp[i].addend;
	  if (i && kind == bitint_prec_large)
	    a = int_const_binop (PLUS_EXPR, a, integer_minus_one_node);
	  if (i)
	    add_phi_arg (phi3, a, bqp[2 * i].e, UNKNOWN_LOCATION);
	}
      add_phi_arg (phi3, build_int_cst (integer_type_node, prec - 1), e,
		   UNKNOWN_LOCATION);
      m_gsi = gsi_after_labels (edge_bb);
      g = gimple_build_call (fndecl, 1,
			     add_cast (signed_type_for (m_limb_type),
				       gimple_phi_result (phi1)));
      gimple_call_set_lhs (g, make_ssa_name (integer_type_node));
      insert_before (g);
      g = gimple_build_assign (make_ssa_name (integer_type_node),
			       PLUS_EXPR, gimple_call_lhs (g),
			       gimple_phi_result (phi2));
      insert_before (g);
      if (kind != bitint_prec_large)
	{
	  g = gimple_build_assign (make_ssa_name (integer_type_node),
				   PLUS_EXPR, gimple_assign_lhs (g),
				   integer_one_node);
	  insert_before (g);
	}
      add_phi_arg (phi3, gimple_assign_lhs (g),
		   find_edge (edge_bb, gimple_bb (stmt)), UNKNOWN_LOCATION);
      m_gsi = gsi_for_stmt (stmt);
      g = gimple_build_assign (lhs, gimple_phi_result (phi3));
      gsi_replace (&m_gsi, g, true);
      break;
    case IFN_PARITY:
      g = gimple_build_call (fndecl, 1, res);
      gimple_call_set_lhs (g, lhs);
      gsi_replace (&m_gsi, g, true);
      break;
    case IFN_POPCOUNT:
      g = gimple_build_assign (lhs, res);
      gsi_replace (&m_gsi, g, true);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Lower a call statement with one or more large/huge _BitInt
   arguments or large/huge _BitInt return value.  */

void
bitint_large_huge::lower_call (tree obj, gimple *stmt)
{
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  unsigned int nargs = gimple_call_num_args (stmt);
  if (gimple_call_internal_p (stmt))
    switch (gimple_call_internal_fn (stmt))
      {
      case IFN_ADD_OVERFLOW:
      case IFN_SUB_OVERFLOW:
      case IFN_UBSAN_CHECK_ADD:
      case IFN_UBSAN_CHECK_SUB:
	lower_addsub_overflow (obj, stmt);
	return;
      case IFN_MUL_OVERFLOW:
      case IFN_UBSAN_CHECK_MUL:
	lower_mul_overflow (obj, stmt);
	return;
      case IFN_CLZ:
      case IFN_CTZ:
      case IFN_CLRSB:
      case IFN_FFS:
      case IFN_PARITY:
      case IFN_POPCOUNT:
	lower_bit_query (stmt);
	return;
      default:
	break;
      }
  bool returns_twice = (gimple_call_flags (stmt) & ECF_RETURNS_TWICE) != 0;
  for (unsigned int i = 0; i < nargs; ++i)
    {
      tree arg = gimple_call_arg (stmt, i);
      if (TREE_CODE (arg) != SSA_NAME
	  || TREE_CODE (TREE_TYPE (arg)) != BITINT_TYPE
	  || bitint_precision_kind (TREE_TYPE (arg)) <= bitint_prec_middle)
	continue;
      if (SSA_NAME_IS_DEFAULT_DEF (arg)
	  && (!SSA_NAME_VAR (arg) || VAR_P (SSA_NAME_VAR (arg))))
	{
	  tree var = create_tmp_reg (TREE_TYPE (arg));
	  arg = get_or_create_ssa_default_def (cfun, var);
	}
      else
	{
	  int p = var_to_partition (m_map, arg);
	  tree v = m_vars[p];
	  gcc_assert (v != NULL_TREE);
	  if (!types_compatible_p (TREE_TYPE (arg), TREE_TYPE (v)))
	    v = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (arg), v);
	  arg = make_ssa_name (TREE_TYPE (arg));
	  gimple *g = gimple_build_assign (arg, v);
	  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
	  if (returns_twice && bb_has_abnormal_pred (gimple_bb (stmt)))
	    {
	      m_returns_twice_calls.safe_push (stmt);
	      returns_twice = false;
	    }
	}
      gimple_call_set_arg (stmt, i, arg);
      if (m_preserved == NULL)
	m_preserved = BITMAP_ALLOC (NULL);
      bitmap_set_bit (m_preserved, SSA_NAME_VERSION (arg));
    }
  tree lhs = gimple_call_lhs (stmt);
  if (lhs
      && TREE_CODE (lhs) == SSA_NAME
      && TREE_CODE (TREE_TYPE (lhs)) == BITINT_TYPE
      && bitint_precision_kind (TREE_TYPE (lhs)) >= bitint_prec_large)
    {
      int p = var_to_partition (m_map, lhs);
      tree v = m_vars[p];
      gcc_assert (v != NULL_TREE);
      if (!types_compatible_p (TREE_TYPE (lhs), TREE_TYPE (v)))
	v = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (lhs), v);
      gimple_call_set_lhs (stmt, v);
      SSA_NAME_DEF_STMT (lhs) = gimple_build_nop ();
    }
  update_stmt (stmt);
}

/* Lower __asm STMT which involves large/huge _BitInt values.  */

void
bitint_large_huge::lower_asm (gimple *stmt)
{
  gasm *g = as_a <gasm *> (stmt);
  unsigned noutputs = gimple_asm_noutputs (g);
  unsigned ninputs = gimple_asm_ninputs (g);

  for (unsigned i = 0; i < noutputs; ++i)
    {
      tree t = gimple_asm_output_op (g, i);
      tree s = TREE_VALUE (t);
      if (TREE_CODE (s) == SSA_NAME
	  && TREE_CODE (TREE_TYPE (s)) == BITINT_TYPE
	  && bitint_precision_kind (TREE_TYPE (s)) >= bitint_prec_large)
	{
	  int part = var_to_partition (m_map, s);
	  gcc_assert (m_vars[part] != NULL_TREE);
	  TREE_VALUE (t) = m_vars[part];
	}
    }
  for (unsigned i = 0; i < ninputs; ++i)
    {
      tree t = gimple_asm_input_op (g, i);
      tree s = TREE_VALUE (t);
      if (TREE_CODE (s) == SSA_NAME
	  && TREE_CODE (TREE_TYPE (s)) == BITINT_TYPE
	  && bitint_precision_kind (TREE_TYPE (s)) >= bitint_prec_large)
	{
	  if (SSA_NAME_IS_DEFAULT_DEF (s)
	      && (!SSA_NAME_VAR (s) || VAR_P (SSA_NAME_VAR (s))))
	    {
	      TREE_VALUE (t) = create_tmp_var (TREE_TYPE (s), "bitint");
	      mark_addressable (TREE_VALUE (t));
	    }
	  else
	    {
	      int part = var_to_partition (m_map, s);
	      gcc_assert (m_vars[part] != NULL_TREE);
	      TREE_VALUE (t) = m_vars[part];
	    }
	}
    }
  update_stmt (stmt);
}

/* Lower statement STMT which involves large/huge _BitInt values
   into code accessing individual limbs.  */

void
bitint_large_huge::lower_stmt (gimple *stmt)
{
  m_first = true;
  m_lhs = NULL_TREE;
  m_data.truncate (0);
  m_data_cnt = 0;
  m_gsi = gsi_for_stmt (stmt);
  m_after_stmt = NULL;
  m_bb = NULL;
  m_init_gsi = m_gsi;
  gsi_prev (&m_init_gsi);
  m_preheader_bb = NULL;
  m_upwards_2limb = 0;
  m_upwards = false;
  m_var_msb = false;
  m_cast_conditional = false;
  m_bitfld_load = 0;
  m_loc = gimple_location (stmt);
  if (is_gimple_call (stmt))
    {
      lower_call (NULL_TREE, stmt);
      return;
    }
  if (gimple_code (stmt) == GIMPLE_ASM)
    {
      lower_asm (stmt);
      return;
    }
  tree lhs = NULL_TREE, cmp_op1 = NULL_TREE, cmp_op2 = NULL_TREE;
  tree_code cmp_code = comparison_op (stmt, &cmp_op1, &cmp_op2);
  bool eq_p = (cmp_code == EQ_EXPR || cmp_code == NE_EXPR);
  bool mergeable_cast_p = false;
  bool final_cast_p = false;
  if (gimple_assign_cast_p (stmt))
    {
      lhs = gimple_assign_lhs (stmt);
      tree rhs1 = gimple_assign_rhs1 (stmt);
      if (TREE_CODE (rhs1) == VIEW_CONVERT_EXPR)
	rhs1 = TREE_OPERAND (rhs1, 0);
      if (TREE_CODE (TREE_TYPE (lhs)) == BITINT_TYPE
	  && bitint_precision_kind (TREE_TYPE (lhs)) >= bitint_prec_large
	  && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	mergeable_cast_p = true;
      else if (TREE_CODE (TREE_TYPE (rhs1)) == BITINT_TYPE
	       && bitint_precision_kind (TREE_TYPE (rhs1)) >= bitint_prec_large
	       && (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
		   || POINTER_TYPE_P (TREE_TYPE (lhs))
		   || gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR))
	{
	  final_cast_p = true;
	  if (((TREE_CODE (TREE_TYPE (lhs)) == INTEGER_TYPE
		&& TYPE_PRECISION (TREE_TYPE (lhs)) > MAX_FIXED_MODE_SIZE)
	       || (!INTEGRAL_TYPE_P (TREE_TYPE (lhs))
		   && !POINTER_TYPE_P (TREE_TYPE (lhs))))
	      && gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR)
	    {
	      /* Handle VIEW_CONVERT_EXPRs to not generally supported
		 huge INTEGER_TYPEs like uint256_t or uint512_t.  These
		 are usually emitted from memcpy folding and backends
		 support moves with them but that is usually it.
		 Similarly handle VCEs to vector/complex types etc.  */
	      gcc_assert (TREE_CODE (rhs1) == SSA_NAME);
	      if (SSA_NAME_IS_DEFAULT_DEF (rhs1)
		  && (!SSA_NAME_VAR (rhs1) || VAR_P (SSA_NAME_VAR (rhs1))))
		{
		  tree var = create_tmp_reg (TREE_TYPE (lhs));
		  rhs1 = get_or_create_ssa_default_def (cfun, var);
		  gimple_assign_set_rhs1 (stmt, rhs1);
		  gimple_assign_set_rhs_code (stmt, SSA_NAME);
		}
	      else if (m_names == NULL
		       || !bitmap_bit_p (m_names, SSA_NAME_VERSION (rhs1)))
		{
		  gimple *g = SSA_NAME_DEF_STMT (rhs1);
		  gcc_assert (gimple_assign_load_p (g));
		  tree mem = gimple_assign_rhs1 (g);
		  tree ltype = TREE_TYPE (lhs);
		  addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (mem));
		  if (as != TYPE_ADDR_SPACE (ltype))
		    ltype
		      = build_qualified_type (ltype,
					      TYPE_QUALS (ltype)
					      | ENCODE_QUAL_ADDR_SPACE (as));
		  rhs1 = build1 (VIEW_CONVERT_EXPR, ltype, unshare_expr (mem));
		  gimple_assign_set_rhs1 (stmt, rhs1);
		}
	      else
		{
		  int part = var_to_partition (m_map, rhs1);
		  gcc_assert (m_vars[part] != NULL_TREE);
		  rhs1 = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (lhs),
				 m_vars[part]);
		  gimple_assign_set_rhs1 (stmt, rhs1);
		}
	      update_stmt (stmt);
	      return;
	    }
	  if (TREE_CODE (rhs1) == SSA_NAME
	      && (m_names == NULL
		  || !bitmap_bit_p (m_names, SSA_NAME_VERSION (rhs1))))
	    {
	      gimple *g = SSA_NAME_DEF_STMT (rhs1);
	      if (is_gimple_assign (g)
		  && gimple_assign_rhs_code (g) == IMAGPART_EXPR)
		{
		  tree rhs2 = TREE_OPERAND (gimple_assign_rhs1 (g), 0);
		  if (TREE_CODE (rhs2) == SSA_NAME
		      && (m_names == NULL
			  || !bitmap_bit_p (m_names, SSA_NAME_VERSION (rhs2))))
		    {
		      g = SSA_NAME_DEF_STMT (rhs2);
		      int ovf = optimizable_arith_overflow (g);
		      if (ovf == 2)
			/* If .{ADD,SUB,MUL}_OVERFLOW has both REALPART_EXPR
			   and IMAGPART_EXPR uses, where the latter is cast to
			   non-_BitInt, it will be optimized when handling
			   the REALPART_EXPR.  */
			return;
		      if (ovf == 1)
			{
			  lower_call (NULL_TREE, g);
			  return;
			}
		    }
		}
	    }
	}
      else if (TREE_CODE (TREE_TYPE (lhs)) == BITINT_TYPE
	       && bitint_precision_kind (TREE_TYPE (lhs)) >= bitint_prec_large
	       && !INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
	       && !POINTER_TYPE_P (TREE_TYPE (rhs1))
	       && gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR)
	{
	  int part = var_to_partition (m_map, lhs);
	  gcc_assert (m_vars[part] != NULL_TREE);
	  lhs = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (rhs1), m_vars[part]);
	  insert_before (gimple_build_assign (lhs, rhs1));
	  return;
	}
    }
  if (gimple_store_p (stmt))
    {
      tree rhs1 = gimple_assign_rhs1 (stmt);
      if (TREE_CODE (rhs1) == SSA_NAME
	  && (m_names == NULL
	      || !bitmap_bit_p (m_names, SSA_NAME_VERSION (rhs1))))
	{
	  gimple *g = SSA_NAME_DEF_STMT (rhs1);
	  m_loc = gimple_location (g);
	  lhs = gimple_assign_lhs (stmt);
	  if (is_gimple_assign (g) && !mergeable_op (g))
	    switch (gimple_assign_rhs_code (g))
	      {
	      case LSHIFT_EXPR:
	      case RSHIFT_EXPR:
		lower_shift_stmt (lhs, g);
	      handled:
		m_gsi = gsi_for_stmt (stmt);
		unlink_stmt_vdef (stmt);
		release_ssa_name (gimple_vdef (stmt));
		gsi_remove (&m_gsi, true);
		return;
	      case MULT_EXPR:
	      case TRUNC_DIV_EXPR:
	      case TRUNC_MOD_EXPR:
		lower_muldiv_stmt (lhs, g);
		goto handled;
	      case FIX_TRUNC_EXPR:
		lower_float_conv_stmt (lhs, g);
		goto handled;
	      case REALPART_EXPR:
	      case IMAGPART_EXPR:
		lower_cplxpart_stmt (lhs, g);
		goto handled;
	      case VIEW_CONVERT_EXPR:
		{
		  tree rhs1 = gimple_assign_rhs1 (g);
		  rhs1 = TREE_OPERAND (rhs1, 0);
		  if (!INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
		      && !POINTER_TYPE_P (TREE_TYPE (rhs1)))
		    {
		      tree ltype = TREE_TYPE (rhs1);
		      addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (lhs));
		      ltype
			= build_qualified_type (ltype,
						TYPE_QUALS (TREE_TYPE (lhs))
						| ENCODE_QUAL_ADDR_SPACE (as));
		      lhs = build1 (VIEW_CONVERT_EXPR, ltype, lhs);
		      gimple_assign_set_lhs (stmt, lhs);
		      gimple_assign_set_rhs1 (stmt, rhs1);
		      gimple_assign_set_rhs_code (stmt, TREE_CODE (rhs1));
		      update_stmt (stmt);
		      return;
		    }
		}
		break;
	      default:
		break;
	      }
	  else if (optimizable_arith_overflow (g) == 3)
	    {
	      lower_call (lhs, g);
	      goto handled;
	    }
	  m_loc = gimple_location (stmt);
	}
    }
  if (mergeable_op (stmt)
      || gimple_store_p (stmt)
      || gimple_assign_load_p (stmt)
      || eq_p
      || mergeable_cast_p)
    {
      lhs = lower_mergeable_stmt (stmt, cmp_code, cmp_op1, cmp_op2);
      if (!eq_p)
	return;
    }
  else if (cmp_code != ERROR_MARK)
    lhs = lower_comparison_stmt (stmt, cmp_code, cmp_op1, cmp_op2);
  if (cmp_code != ERROR_MARK)
    {
      if (gimple_code (stmt) == GIMPLE_COND)
	{
	  gcond *cstmt = as_a <gcond *> (stmt);
	  gimple_cond_set_lhs (cstmt, lhs);
	  gimple_cond_set_rhs (cstmt, boolean_false_node);
	  gimple_cond_set_code (cstmt, cmp_code);
	  update_stmt (stmt);
	  return;
	}
      if (gimple_assign_rhs_code (stmt) == COND_EXPR)
	{
	  tree cond = build2 (cmp_code, boolean_type_node, lhs,
			      boolean_false_node);
	  gimple_assign_set_rhs1 (stmt, cond);
	  lhs = gimple_assign_lhs (stmt);
	  gcc_assert (TREE_CODE (TREE_TYPE (lhs)) != BITINT_TYPE
		      || (bitint_precision_kind (TREE_TYPE (lhs))
			  <= bitint_prec_middle));
	  update_stmt (stmt);
	  return;
	}
      gimple_assign_set_rhs1 (stmt, lhs);
      gimple_assign_set_rhs2 (stmt, boolean_false_node);
      gimple_assign_set_rhs_code (stmt, cmp_code);
      update_stmt (stmt);
      return;
    }
  if (final_cast_p)
    {
      tree lhs_type = TREE_TYPE (lhs);
      /* Add support for 3 or more limbs filled in from normal integral
	 type if this assert fails.  If no target chooses limb mode smaller
	 than half of largest supported normal integral type, this will not
	 be needed.  */
      gcc_assert (TYPE_PRECISION (lhs_type) <= 2 * limb_prec);
      gimple *g;
      if ((TREE_CODE (lhs_type) == BITINT_TYPE
	   && bitint_precision_kind (lhs_type) == bitint_prec_middle)
	  || POINTER_TYPE_P (lhs_type))
	lhs_type = build_nonstandard_integer_type (TYPE_PRECISION (lhs_type),
						   TYPE_UNSIGNED (lhs_type));
      m_data_cnt = 0;
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree r1 = handle_operand (rhs1, size_int (0));
      if (!useless_type_conversion_p (lhs_type, TREE_TYPE (r1)))
	r1 = add_cast (lhs_type, r1);
      if (TYPE_PRECISION (lhs_type) > limb_prec)
	{
	  m_data_cnt = 0;
	  m_first = false;
	  tree r2 = handle_operand (rhs1, size_int (1));
	  r2 = add_cast (lhs_type, r2);
	  g = gimple_build_assign (make_ssa_name (lhs_type), LSHIFT_EXPR, r2,
				   build_int_cst (unsigned_type_node,
						  limb_prec));
	  insert_before (g);
	  g = gimple_build_assign (make_ssa_name (lhs_type), BIT_IOR_EXPR, r1,
				   gimple_assign_lhs (g));
	  insert_before (g);
	  r1 = gimple_assign_lhs (g);
	}
      if (lhs_type != TREE_TYPE (lhs))
	g = gimple_build_assign (lhs, NOP_EXPR, r1);
      else
	g = gimple_build_assign (lhs, r1);
      gsi_replace (&m_gsi, g, true);
      return;
    }
  if (is_gimple_assign (stmt))
    switch (gimple_assign_rhs_code (stmt))
      {
      case LSHIFT_EXPR:
      case RSHIFT_EXPR:
	lower_shift_stmt (NULL_TREE, stmt);
	return;
      case MULT_EXPR:
      case TRUNC_DIV_EXPR:
      case TRUNC_MOD_EXPR:
	lower_muldiv_stmt (NULL_TREE, stmt);
	return;
      case FIX_TRUNC_EXPR:
      case FLOAT_EXPR:
	lower_float_conv_stmt (NULL_TREE, stmt);
	return;
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	lower_cplxpart_stmt (NULL_TREE, stmt);
	return;
      case COMPLEX_EXPR:
	lower_complexexpr_stmt (stmt);
	return;
      default:
	break;
      }
  gcc_unreachable ();
}

/* Helper for walk_non_aliased_vuses.  Determine if we arrived at
   the desired memory state.  */

void *
vuse_eq (ao_ref *, tree vuse1, void *data)
{
  tree vuse2 = (tree) data;
  if (vuse1 == vuse2)
    return data;

  return NULL;
}

/* Return true if STMT uses a library function and needs to take
   address of its inputs.  We need to avoid bit-fields in those
   cases.  Similarly, we need to avoid overlap between destination
   and source limb arrays.  */

bool
stmt_needs_operand_addr (gimple *stmt)
{
  if (is_gimple_assign (stmt))
    switch (gimple_assign_rhs_code (stmt))
      {
      case MULT_EXPR:
      case TRUNC_DIV_EXPR:
      case TRUNC_MOD_EXPR:
      case FLOAT_EXPR:
	return true;
      default:
	break;
      }
  else if (gimple_call_internal_p (stmt, IFN_MUL_OVERFLOW)
	   || gimple_call_internal_p (stmt, IFN_UBSAN_CHECK_MUL))
    return true;
  return false;
}

/* Dominator walker used to discover which large/huge _BitInt
   loads could be sunk into all their uses.  */

class bitint_dom_walker : public dom_walker
{
public:
  bitint_dom_walker (bitmap names, bitmap loads)
    : dom_walker (CDI_DOMINATORS), m_names (names), m_loads (loads) {}

  edge before_dom_children (basic_block) final override;

private:
  bitmap m_names, m_loads;
};

edge
bitint_dom_walker::before_dom_children (basic_block bb)
{
  gphi *phi = get_virtual_phi (bb);
  tree vop;
  if (phi)
    vop = gimple_phi_result (phi);
  else if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun))
    vop = NULL_TREE;
  else
    vop = (tree) get_immediate_dominator (CDI_DOMINATORS, bb)->aux;

  auto_vec<tree, 16> worklist;
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (is_gimple_debug (stmt))
	continue;

      if (!vop && gimple_vuse (stmt))
	vop = gimple_vuse (stmt);

      tree cvop = vop;
      if (gimple_vdef (stmt))
	vop = gimple_vdef (stmt);

      tree lhs = gimple_get_lhs (stmt);
      if (lhs
	  && TREE_CODE (lhs) == SSA_NAME
	  && TREE_CODE (TREE_TYPE (lhs)) == BITINT_TYPE
	  && bitint_precision_kind (TREE_TYPE (lhs)) >= bitint_prec_large
	  && !bitmap_bit_p (m_names, SSA_NAME_VERSION (lhs)))
	/* If lhs of stmt is large/huge _BitInt SSA_NAME not in m_names,
	   it means it will be handled in a loop or straight line code
	   at the location of its (ultimate) immediate use, so for
	   vop checking purposes check these only at the ultimate
	   immediate use.  */
	continue;

      ssa_op_iter oi;
      use_operand_p use_p;
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, oi, SSA_OP_USE)
	{
	  tree s = USE_FROM_PTR (use_p);
	  if (TREE_CODE (TREE_TYPE (s)) == BITINT_TYPE
	      && bitint_precision_kind (TREE_TYPE (s)) >= bitint_prec_large)
	    worklist.safe_push (s);
	}

      bool needs_operand_addr = stmt_needs_operand_addr (stmt);
      while (worklist.length () > 0)
	{
	  tree s = worklist.pop ();

	  if (!bitmap_bit_p (m_names, SSA_NAME_VERSION (s)))
	    {
	      gimple *g = SSA_NAME_DEF_STMT (s);
	      needs_operand_addr |= stmt_needs_operand_addr (g);
	      FOR_EACH_SSA_USE_OPERAND (use_p, g, oi, SSA_OP_USE)
		{
		  tree s2 = USE_FROM_PTR (use_p);
		  if (TREE_CODE (TREE_TYPE (s2)) == BITINT_TYPE
		      && (bitint_precision_kind (TREE_TYPE (s2))
			  >= bitint_prec_large))
		    worklist.safe_push (s2);
		}
	      continue;
	    }
	  if (!SSA_NAME_OCCURS_IN_ABNORMAL_PHI (s)
	      && gimple_assign_cast_p (SSA_NAME_DEF_STMT (s)))
	    {
	      tree rhs = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (s));
	      if (TREE_CODE (rhs) == SSA_NAME
		  && bitmap_bit_p (m_loads, SSA_NAME_VERSION (rhs)))
		s = rhs;
	      else
		continue;
	    }
	  else if (!bitmap_bit_p (m_loads, SSA_NAME_VERSION (s)))
	    continue;

	  gimple *g = SSA_NAME_DEF_STMT (s);
	  tree rhs1 = gimple_assign_rhs1 (g);
	  if (needs_operand_addr
	      && TREE_CODE (rhs1) == COMPONENT_REF
	      && DECL_BIT_FIELD_TYPE (TREE_OPERAND (rhs1, 1)))
	    {
	      tree fld = TREE_OPERAND (rhs1, 1);
	      /* For little-endian, we can allow as inputs bit-fields
		 which start at a limb boundary.  */
	      if (DECL_OFFSET_ALIGN (fld) >= TYPE_ALIGN (TREE_TYPE (rhs1))
		  && tree_fits_uhwi_p (DECL_FIELD_BIT_OFFSET (fld))
		  && (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (fld))
		      % limb_prec) == 0)
		;
	      else
		{
		  bitmap_clear_bit (m_loads, SSA_NAME_VERSION (s));
		  continue;
		}
	    }

	  ao_ref ref;
	  ao_ref_init (&ref, rhs1);
	  tree lvop = gimple_vuse (g);
	  unsigned limit = 64;
	  tree vuse = cvop;
	  if (vop != cvop
	      && is_gimple_assign (stmt)
	      && gimple_store_p (stmt)
	      && (needs_operand_addr
		  || !operand_equal_p (lhs, gimple_assign_rhs1 (g), 0)))
	    vuse = vop;
	  if (vuse != lvop
	      && walk_non_aliased_vuses (&ref, vuse, false, vuse_eq,
					 NULL, NULL, limit, lvop) == NULL)
	    bitmap_clear_bit (m_loads, SSA_NAME_VERSION (s));
	}
    }

  bb->aux = (void *) vop;
  return NULL;
}

}

/* Replacement for normal processing of STMT in tree-ssa-coalesce.cc
   build_ssa_conflict_graph.
   The differences are:
   1) don't process assignments with large/huge _BitInt lhs not in NAMES
   2) for large/huge _BitInt multiplication/division/modulo process def
      only after processing uses rather than before to make uses conflict
      with the definition
   3) for large/huge _BitInt uses not in NAMES mark the uses of their
      SSA_NAME_DEF_STMT (recursively), because those uses will be sunk into
      the final statement.  */

void
build_bitint_stmt_ssa_conflicts (gimple *stmt, live_track *live,
				 ssa_conflicts *graph, bitmap names,
				 void (*def) (live_track *, tree,
					      ssa_conflicts *),
				 void (*use) (live_track *, tree))
{
  bool muldiv_p = false;
  tree lhs = NULL_TREE;
  if (is_gimple_assign (stmt))
    {
      lhs = gimple_assign_lhs (stmt);
      if (TREE_CODE (lhs) == SSA_NAME)
	{
	  tree type = TREE_TYPE (lhs);
	  if (TREE_CODE (type) == COMPLEX_TYPE)
	    type = TREE_TYPE (type);
	  if (TREE_CODE (type) == BITINT_TYPE
	      && bitint_precision_kind (type) >= bitint_prec_large)
	    {
	      if (!bitmap_bit_p (names, SSA_NAME_VERSION (lhs)))
		return;
	      switch (gimple_assign_rhs_code (stmt))
		{
		case MULT_EXPR:
		case TRUNC_DIV_EXPR:
		case TRUNC_MOD_EXPR:
		  muldiv_p = true;
		default:
		  break;
		}
	    }
	}
    }

  ssa_op_iter iter;
  tree var;
  if (!muldiv_p)
    {
      /* For stmts with more than one SSA_NAME definition pretend all the
	 SSA_NAME outputs but the first one are live at this point, so
	 that conflicts are added in between all those even when they are
	 actually not really live after the asm, because expansion might
	 copy those into pseudos after the asm and if multiple outputs
	 share the same partition, it might overwrite those that should
	 be live.  E.g.
	 asm volatile (".." : "=r" (a) : "=r" (b) : "0" (a), "1" (a));
	 return a;
	 See PR70593.  */
      bool first = true;
      FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_DEF)
	if (first)
	  first = false;
	else
	  use (live, var);

      FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_DEF)
	def (live, var, graph);
    }

  auto_vec<tree, 16> worklist;
  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_USE)
    {
      tree type = TREE_TYPE (var);
      if (TREE_CODE (type) == COMPLEX_TYPE)
	type = TREE_TYPE (type);
      if (TREE_CODE (type) == BITINT_TYPE
	  && bitint_precision_kind (type) >= bitint_prec_large)
	{
	  if (bitmap_bit_p (names, SSA_NAME_VERSION (var)))
	    use (live, var);
	  else
	    worklist.safe_push (var);
	}
    }

  while (worklist.length () > 0)
    {
      tree s = worklist.pop ();
      FOR_EACH_SSA_TREE_OPERAND (var, SSA_NAME_DEF_STMT (s), iter, SSA_OP_USE)
	{
	  tree type = TREE_TYPE (var);
	  if (TREE_CODE (type) == COMPLEX_TYPE)
	    type = TREE_TYPE (type);
	  if (TREE_CODE (type) == BITINT_TYPE
	      && bitint_precision_kind (type) >= bitint_prec_large)
	    {
	      if (bitmap_bit_p (names, SSA_NAME_VERSION (var)))
		use (live, var);
	      else
		worklist.safe_push (var);
	    }
	}
    }

  if (muldiv_p)
    def (live, lhs, graph);
}

/* If STMT is .{ADD,SUB,MUL}_OVERFLOW with INTEGER_CST arguments,
   return the largest bitint_prec_kind of them, otherwise return
   bitint_prec_small.  */

static bitint_prec_kind
arith_overflow_arg_kind (gimple *stmt)
{
  bitint_prec_kind ret = bitint_prec_small;
  if (is_gimple_call (stmt) && gimple_call_internal_p (stmt))
    switch (gimple_call_internal_fn (stmt))
      {
      case IFN_ADD_OVERFLOW:
      case IFN_SUB_OVERFLOW:
      case IFN_MUL_OVERFLOW:
	for (int i = 0; i < 2; ++i)
	  {
	    tree a = gimple_call_arg (stmt, i);
	    if (TREE_CODE (a) == INTEGER_CST
		&& TREE_CODE (TREE_TYPE (a)) == BITINT_TYPE)
	      {
		bitint_prec_kind kind = bitint_precision_kind (TREE_TYPE (a));
		ret = MAX (ret, kind);
	      }
	  }
	break;
      default:
	break;
      }
  return ret;
}

/* Entry point for _BitInt(N) operation lowering during optimization.  */

static unsigned int
gimple_lower_bitint (void)
{
  small_max_prec = mid_min_prec = large_min_prec = huge_min_prec = 0;
  limb_prec = 0;

  unsigned int i;
  for (i = 0; i < num_ssa_names; ++i)
    {
      tree s = ssa_name (i);
      if (s == NULL)
	continue;
      tree type = TREE_TYPE (s);
      if (TREE_CODE (type) == COMPLEX_TYPE)
	{
	  if (arith_overflow_arg_kind (SSA_NAME_DEF_STMT (s))
	      != bitint_prec_small)
	    break;
	  type = TREE_TYPE (type);
	}
      if (TREE_CODE (type) == BITINT_TYPE
	  && bitint_precision_kind (type) != bitint_prec_small)
	break;
      /* We need to also rewrite stores of large/huge _BitInt INTEGER_CSTs
	 into memory.  Such functions could have no large/huge SSA_NAMEs.  */
      if (SSA_NAME_IS_VIRTUAL_OPERAND (s))
	{
	  gimple *g = SSA_NAME_DEF_STMT (s);
	  if (is_gimple_assign (g) && gimple_store_p (g))
	    {
	      tree t = gimple_assign_rhs1 (g);
	      if (TREE_CODE (TREE_TYPE (t)) == BITINT_TYPE
		  && (bitint_precision_kind (TREE_TYPE (t))
		      >= bitint_prec_large))
		break;
	    }
	}
      /* Similarly, e.g. with -frounding-math casts from _BitInt INTEGER_CSTs
	 to floating point types need to be rewritten.  */
      else if (SCALAR_FLOAT_TYPE_P (type))
	{
	  gimple *g = SSA_NAME_DEF_STMT (s);
	  if (is_gimple_assign (g) && gimple_assign_rhs_code (g) == FLOAT_EXPR)
	    {
	      tree t = gimple_assign_rhs1 (g);
	      if (TREE_CODE (t) == INTEGER_CST
		  && TREE_CODE (TREE_TYPE (t)) == BITINT_TYPE
		  && (bitint_precision_kind (TREE_TYPE (t))
		      != bitint_prec_small))
		break;
	    }
	}
    }
  if (i == num_ssa_names)
    return 0;

  basic_block bb;
  auto_vec<gimple *, 4> switch_statements;
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (gswitch *swtch = safe_dyn_cast <gswitch *> (*gsi_last_bb (bb)))
	{
	  tree idx = gimple_switch_index (swtch);
	  if (TREE_CODE (TREE_TYPE (idx)) != BITINT_TYPE
	      || bitint_precision_kind (TREE_TYPE (idx)) < bitint_prec_large)
	    continue;

	  if (optimize)
	    group_case_labels_stmt (swtch);
	  if (gimple_switch_num_labels (swtch) == 1)
	    {
	      single_succ_edge (bb)->flags |= EDGE_FALLTHRU;
	      gimple_stmt_iterator gsi = gsi_for_stmt (swtch);
	      gsi_remove (&gsi, true);
	    }
	  else
	    switch_statements.safe_push (swtch);
	}
    }

  if (!switch_statements.is_empty ())
    {
      bool expanded = false;
      gimple *stmt;
      unsigned int j;
      i = 0;
      FOR_EACH_VEC_ELT (switch_statements, j, stmt)
	{
	  gswitch *swtch = as_a<gswitch *> (stmt);
	  tree_switch_conversion::switch_decision_tree dt (swtch);
	  expanded |= dt.analyze_switch_statement ();
	}

      if (expanded)
	{
	  free_dominance_info (CDI_DOMINATORS);
	  free_dominance_info (CDI_POST_DOMINATORS);
	  mark_virtual_operands_for_renaming (cfun);
	  cleanup_tree_cfg (TODO_update_ssa);
	}
    }

  struct bitint_large_huge large_huge;
  bool has_large_huge_parm_result = false;
  bool has_large_huge = false;
  unsigned int ret = 0, first_large_huge = ~0U;
  bool edge_insertions = false;
  for (; i < num_ssa_names; ++i)
    {
      tree s = ssa_name (i);
      if (s == NULL)
	continue;
      tree type = TREE_TYPE (s);
      if (TREE_CODE (type) == COMPLEX_TYPE)
	{
	  if (arith_overflow_arg_kind (SSA_NAME_DEF_STMT (s))
	      >= bitint_prec_large)
	    has_large_huge = true;
	  type = TREE_TYPE (type);
	}
      if (TREE_CODE (type) == BITINT_TYPE
	  && bitint_precision_kind (type) >= bitint_prec_large)
	{
	  if (first_large_huge == ~0U)
	    first_large_huge = i;
	  gimple *stmt = SSA_NAME_DEF_STMT (s), *g;
	  gimple_stmt_iterator gsi;
	  tree_code rhs_code;
	  /* Unoptimize certain constructs to simpler alternatives to
	     avoid having to lower all of them.  */
	  if (is_gimple_assign (stmt) && gimple_bb (stmt))
	    switch (rhs_code = gimple_assign_rhs_code (stmt))
	      {
	      default:
		break;
	      case MULT_EXPR:
	      case TRUNC_DIV_EXPR:
	      case TRUNC_MOD_EXPR:
		if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (s))
		  {
		    location_t loc = gimple_location (stmt);
		    gsi = gsi_for_stmt (stmt);
		    tree rhs1 = gimple_assign_rhs1 (stmt);
		    tree rhs2 = gimple_assign_rhs2 (stmt);
		    /* For multiplication and division with (ab)
		       lhs and one or both operands force the operands
		       into new SSA_NAMEs to avoid coalescing failures.  */
		    if (TREE_CODE (rhs1) == SSA_NAME
			&& SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs1))
		      {
			first_large_huge = 0;
			tree t = make_ssa_name (TREE_TYPE (rhs1));
			g = gimple_build_assign (t, SSA_NAME, rhs1);
			gsi_insert_before (&gsi, g, GSI_SAME_STMT);
			gimple_set_location (g, loc);
			gimple_assign_set_rhs1 (stmt, t);
			if (rhs1 == rhs2)
			  {
			    gimple_assign_set_rhs2 (stmt, t);
			    rhs2 = t;
			  }
			update_stmt (stmt);
		      }
		    if (TREE_CODE (rhs2) == SSA_NAME
			&& SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs2))
		      {
			first_large_huge = 0;
			tree t = make_ssa_name (TREE_TYPE (rhs2));
			g = gimple_build_assign (t, SSA_NAME, rhs2);
			gsi_insert_before (&gsi, g, GSI_SAME_STMT);
			gimple_set_location (g, loc);
			gimple_assign_set_rhs2 (stmt, t);
			update_stmt (stmt);
		      }
		  }
		break;
	      case LROTATE_EXPR:
	      case RROTATE_EXPR:
		{
		  first_large_huge = 0;
		  location_t loc = gimple_location (stmt);
		  gsi = gsi_for_stmt (stmt);
		  tree rhs1 = gimple_assign_rhs1 (stmt);
		  tree type = TREE_TYPE (rhs1);
		  tree n = gimple_assign_rhs2 (stmt), m;
		  tree p = build_int_cst (TREE_TYPE (n),
					  TYPE_PRECISION (type));
		  if (TREE_CODE (n) == INTEGER_CST)
		    m = fold_build2 (MINUS_EXPR, TREE_TYPE (n), p, n);
		  else
		    {
		      m = make_ssa_name (TREE_TYPE (n));
		      g = gimple_build_assign (m, MINUS_EXPR, p, n);
		      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
		      gimple_set_location (g, loc);
		    }
		  if (!TYPE_UNSIGNED (type))
		    {
		      tree utype = build_bitint_type (TYPE_PRECISION (type),
						      1);
		      if (TREE_CODE (rhs1) == INTEGER_CST)
			rhs1 = fold_convert (utype, rhs1);
		      else
			{
			  tree t = make_ssa_name (type);
			  g = gimple_build_assign (t, NOP_EXPR, rhs1);
			  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
			  gimple_set_location (g, loc);
			}
		    }
		  g = gimple_build_assign (make_ssa_name (TREE_TYPE (rhs1)),
					   rhs_code == LROTATE_EXPR
					   ? LSHIFT_EXPR : RSHIFT_EXPR,
					   rhs1, n);
		  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
		  gimple_set_location (g, loc);
		  tree op1 = gimple_assign_lhs (g);
		  g = gimple_build_assign (make_ssa_name (TREE_TYPE (rhs1)),
					   rhs_code == LROTATE_EXPR
					   ? RSHIFT_EXPR : LSHIFT_EXPR,
					   rhs1, m);
		  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
		  gimple_set_location (g, loc);
		  tree op2 = gimple_assign_lhs (g);
		  tree lhs = gimple_assign_lhs (stmt);
		  if (!TYPE_UNSIGNED (type))
		    {
		      g = gimple_build_assign (make_ssa_name (TREE_TYPE (op1)),
					       BIT_IOR_EXPR, op1, op2);
		      gsi_insert_before (&gsi, g, GSI_SAME_STMT);
		      gimple_set_location (g, loc);
		      g = gimple_build_assign (lhs, NOP_EXPR,
					       gimple_assign_lhs (g));
		    }
		  else
		    g = gimple_build_assign (lhs, BIT_IOR_EXPR, op1, op2);
		  gsi_replace (&gsi, g, true);
		  gimple_set_location (g, loc);
		}
		break;
	      case ABS_EXPR:
	      case ABSU_EXPR:
	      case MIN_EXPR:
	      case MAX_EXPR:
	      case COND_EXPR:
		first_large_huge = 0;
		gsi = gsi_for_stmt (stmt);
		tree lhs = gimple_assign_lhs (stmt);
		tree rhs1 = gimple_assign_rhs1 (stmt), rhs2 = NULL_TREE;
		location_t loc = gimple_location (stmt);
		if (rhs_code == ABS_EXPR)
		  g = gimple_build_cond (LT_EXPR, rhs1,
		  			 build_zero_cst (TREE_TYPE (rhs1)),
					 NULL_TREE, NULL_TREE);
		else if (rhs_code == ABSU_EXPR)
		  {
		    rhs2 = make_ssa_name (TREE_TYPE (lhs));
		    g = gimple_build_assign (rhs2, NOP_EXPR, rhs1);
		    gsi_insert_before (&gsi, g, GSI_SAME_STMT);
		    gimple_set_location (g, loc);
		    g = gimple_build_cond (LT_EXPR, rhs1,
					   build_zero_cst (TREE_TYPE (rhs1)),
					   NULL_TREE, NULL_TREE);
		    rhs1 = rhs2;
		  }
		else if (rhs_code == MIN_EXPR || rhs_code == MAX_EXPR)
		  {
		    rhs2 = gimple_assign_rhs2 (stmt);
		    if (TREE_CODE (rhs1) == INTEGER_CST)
		      std::swap (rhs1, rhs2);
		    g = gimple_build_cond (LT_EXPR, rhs1, rhs2,
					   NULL_TREE, NULL_TREE);
		    if (rhs_code == MAX_EXPR)
		      std::swap (rhs1, rhs2);
		  }
		else
		  {
		    g = gimple_build_cond (NE_EXPR, rhs1,
					   build_zero_cst (TREE_TYPE (rhs1)),
					   NULL_TREE, NULL_TREE);
		    rhs1 = gimple_assign_rhs2 (stmt);
		    rhs2 = gimple_assign_rhs3 (stmt);
		  }
		gsi_insert_before (&gsi, g, GSI_SAME_STMT);
		gimple_set_location (g, loc);
		edge e1 = split_block (gsi_bb (gsi), g);
		edge e2 = split_block (e1->dest, (gimple *) NULL);
		edge e3 = make_edge (e1->src, e2->dest, EDGE_FALSE_VALUE);
		e3->probability = profile_probability::even ();
		e1->flags = EDGE_TRUE_VALUE;
		e1->probability = e3->probability.invert ();
		if (dom_info_available_p (CDI_DOMINATORS))
		  set_immediate_dominator (CDI_DOMINATORS, e2->dest, e1->src);
		if (rhs_code == ABS_EXPR || rhs_code == ABSU_EXPR)
		  {
		    gsi = gsi_after_labels (e1->dest);
		    g = gimple_build_assign (make_ssa_name (TREE_TYPE (rhs1)),
					     NEGATE_EXPR, rhs1);
		    gsi_insert_before (&gsi, g, GSI_SAME_STMT);
		    gimple_set_location (g, loc);
		    rhs2 = gimple_assign_lhs (g);
		    std::swap (rhs1, rhs2);
		  }
		gsi = gsi_for_stmt (stmt);
		gsi_remove (&gsi, true);
		gphi *phi = create_phi_node (lhs, e2->dest);
		add_phi_arg (phi, rhs1, e2, UNKNOWN_LOCATION);
		add_phi_arg (phi, rhs2, e3, UNKNOWN_LOCATION);
		break;
	      }
	}
      /* We need to also rewrite stores of large/huge _BitInt INTEGER_CSTs
	 into memory.  Such functions could have no large/huge SSA_NAMEs.  */
      else if (SSA_NAME_IS_VIRTUAL_OPERAND (s))
	{
	  gimple *g = SSA_NAME_DEF_STMT (s);
	  if (is_gimple_assign (g) && gimple_store_p (g))
	    {
	      tree t = gimple_assign_rhs1 (g);
	      if (TREE_CODE (TREE_TYPE (t)) == BITINT_TYPE
		  && (bitint_precision_kind (TREE_TYPE (t))
		      >= bitint_prec_large))
		has_large_huge = true;
	    }
	}
      /* Similarly, e.g. with -frounding-math casts from _BitInt INTEGER_CSTs
	 to floating point types need to be rewritten.  */
      else if (SCALAR_FLOAT_TYPE_P (type))
	{
	  gimple *g = SSA_NAME_DEF_STMT (s);
	  if (is_gimple_assign (g) && gimple_assign_rhs_code (g) == FLOAT_EXPR)
	    {
	      tree t = gimple_assign_rhs1 (g);
	      if (TREE_CODE (t) == INTEGER_CST
		  && TREE_CODE (TREE_TYPE (t)) == BITINT_TYPE
		  && (bitint_precision_kind (TREE_TYPE (t))
		      >= bitint_prec_large))
		has_large_huge = true;
	    }
	}
    }
  for (i = first_large_huge; i < num_ssa_names; ++i)
    {
      tree s = ssa_name (i);
      if (s == NULL)
	continue;
      tree type = TREE_TYPE (s);
      if (TREE_CODE (type) == COMPLEX_TYPE)
	type = TREE_TYPE (type);
      if (TREE_CODE (type) == BITINT_TYPE
	  && bitint_precision_kind (type) >= bitint_prec_large)
	{
	  use_operand_p use_p;
	  gimple *use_stmt;
	  has_large_huge = true;
	  if (optimize
	      && optimizable_arith_overflow (SSA_NAME_DEF_STMT (s)))
	    continue;
	  /* Ignore large/huge _BitInt SSA_NAMEs which have single use in
	     the same bb and could be handled in the same loop with the
	     immediate use.  */
	  if (optimize
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (s)
	      && single_imm_use (s, &use_p, &use_stmt)
	      && gimple_bb (SSA_NAME_DEF_STMT (s)) == gimple_bb (use_stmt))
	    {
	      if (mergeable_op (SSA_NAME_DEF_STMT (s)))
		{
		  if (mergeable_op (use_stmt))
		    continue;
		  tree_code cmp_code = comparison_op (use_stmt, NULL, NULL);
		  if (cmp_code == EQ_EXPR || cmp_code == NE_EXPR)
		    continue;
		  if (gimple_assign_cast_p (use_stmt))
		    {
		      tree lhs = gimple_assign_lhs (use_stmt);
		      if (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
			  /* Don't merge with VIEW_CONVERT_EXPRs to
			     huge INTEGER_TYPEs used sometimes in memcpy
			     expansion.  */
			  && (TREE_CODE (TREE_TYPE (lhs)) != INTEGER_TYPE
			      || (TYPE_PRECISION (TREE_TYPE (lhs))
				  <= MAX_FIXED_MODE_SIZE)))
			continue;
		    }
		  else if (gimple_store_p (use_stmt)
			   && is_gimple_assign (use_stmt)
			   && !gimple_has_volatile_ops (use_stmt)
			   && !stmt_ends_bb_p (use_stmt))
		    continue;
		}
	      if (gimple_assign_cast_p (SSA_NAME_DEF_STMT (s)))
		{
		  tree rhs1 = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (s));
		  if (TREE_CODE (rhs1) == VIEW_CONVERT_EXPR)
		    {
		      rhs1 = TREE_OPERAND (rhs1, 0);
		      if (!INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
			  && !POINTER_TYPE_P (TREE_TYPE (rhs1))
			  && gimple_store_p (use_stmt))
			continue;
		    }
		  if (INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
		      && ((is_gimple_assign (use_stmt)
			   && (gimple_assign_rhs_code (use_stmt)
			       != COMPLEX_EXPR))
			  || gimple_code (use_stmt) == GIMPLE_COND)
		      && (!gimple_store_p (use_stmt)
			  || (is_gimple_assign (use_stmt)
			      && !gimple_has_volatile_ops (use_stmt)
			      && !stmt_ends_bb_p (use_stmt)))
		      && (TREE_CODE (rhs1) != SSA_NAME
			  || !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs1)))
		    {
		      if (is_gimple_assign (use_stmt))
			switch (gimple_assign_rhs_code (use_stmt))
			  {
			  case TRUNC_DIV_EXPR:
			  case TRUNC_MOD_EXPR:
			  case FLOAT_EXPR:
			    /* For division, modulo and casts to floating
			       point, avoid representing unsigned operands
			       using negative prec if they were sign-extended
			       from narrower precision.  */
			    if (TYPE_UNSIGNED (TREE_TYPE (s))
				&& !TYPE_UNSIGNED (TREE_TYPE (rhs1))
				&& (TYPE_PRECISION (TREE_TYPE (s))
				    > TYPE_PRECISION (TREE_TYPE (rhs1))))
			      goto force_name;
			    /* FALLTHRU */
			  case MULT_EXPR:
			    if (TREE_CODE (TREE_TYPE (rhs1)) != BITINT_TYPE
				|| (bitint_precision_kind (TREE_TYPE (rhs1))
				    < bitint_prec_large))
			      continue;
			    /* Uses which use handle_operand_addr can't
			       deal with nested casts.  */
			    if (TREE_CODE (rhs1) == SSA_NAME
				&& gimple_assign_cast_p
				     (SSA_NAME_DEF_STMT (rhs1))
				&& has_single_use (rhs1)
				&& (gimple_bb (SSA_NAME_DEF_STMT (rhs1))
				    == gimple_bb (SSA_NAME_DEF_STMT (s))))
			      goto force_name;
			    break;
			  case VIEW_CONVERT_EXPR:
			    {
			      tree lhs = gimple_assign_lhs (use_stmt);
			      /* Don't merge with VIEW_CONVERT_EXPRs to
				 non-integral types.  */
			      if (!INTEGRAL_TYPE_P (TREE_TYPE (lhs)))
				goto force_name;
			      /* Don't merge with VIEW_CONVERT_EXPRs to
				 huge INTEGER_TYPEs used sometimes in memcpy
				 expansion.  */
			      if (TREE_CODE (TREE_TYPE (lhs)) == INTEGER_TYPE
				  && (TYPE_PRECISION (TREE_TYPE (lhs))
				      > MAX_FIXED_MODE_SIZE))
				goto force_name;
			    }
			    break;
			  default:
			    break;
			}
		      if (TREE_CODE (TREE_TYPE (rhs1)) != BITINT_TYPE
			  || (bitint_precision_kind (TREE_TYPE (rhs1))
			      < bitint_prec_large))
			continue;
		      if ((TYPE_PRECISION (TREE_TYPE (rhs1))
			   >= TYPE_PRECISION (TREE_TYPE (s)))
			  && mergeable_op (use_stmt))
			continue;
		      /* Prevent merging a widening non-mergeable cast
			 on result of some narrower mergeable op
			 together with later mergeable operations.  E.g.
			 result of _BitInt(223) addition shouldn't be
			 sign-extended to _BitInt(513) and have another
			 _BitInt(513) added to it, as handle_plus_minus
			 with its PHI node handling inside of handle_cast
			 will not work correctly.  An exception is if
			 use_stmt is a store, this is handled directly
			 in lower_mergeable_stmt.  */
		      if (TREE_CODE (rhs1) != SSA_NAME
			  || !has_single_use (rhs1)
			  || (gimple_bb (SSA_NAME_DEF_STMT (rhs1))
			      != gimple_bb (SSA_NAME_DEF_STMT (s)))
			  || !mergeable_op (SSA_NAME_DEF_STMT (rhs1))
			  || gimple_store_p (use_stmt))
			continue;
		      if ((TYPE_PRECISION (TREE_TYPE (rhs1))
			   < TYPE_PRECISION (TREE_TYPE (s)))
			  && gimple_assign_cast_p (SSA_NAME_DEF_STMT (rhs1)))
			{
			  /* Another exception is if the widening cast is
			     from mergeable same precision cast from something
			     not mergeable.  */
			  tree rhs2
			    = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (rhs1));
			  if (TREE_CODE (TREE_TYPE (rhs2)) == BITINT_TYPE
			      && (TYPE_PRECISION (TREE_TYPE (rhs1))
				  == TYPE_PRECISION (TREE_TYPE (rhs2))))
			    {
			      if (TREE_CODE (rhs2) != SSA_NAME
				  || !has_single_use (rhs2)
				  || (gimple_bb (SSA_NAME_DEF_STMT (rhs2))
				      != gimple_bb (SSA_NAME_DEF_STMT (s)))
				  || !mergeable_op (SSA_NAME_DEF_STMT (rhs2)))
				continue;
			    }
			}
		    }
		}
	      if (is_gimple_assign (SSA_NAME_DEF_STMT (s)))
		switch (gimple_assign_rhs_code (SSA_NAME_DEF_STMT (s)))
		  {
		  case IMAGPART_EXPR:
		    {
		      tree rhs1 = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (s));
		      rhs1 = TREE_OPERAND (rhs1, 0);
		      if (TREE_CODE (rhs1) == SSA_NAME)
			{
			  gimple *g = SSA_NAME_DEF_STMT (rhs1);
			  if (optimizable_arith_overflow (g))
			    continue;
			}
		    }
		    /* FALLTHRU */
		  case LSHIFT_EXPR:
		  case RSHIFT_EXPR:
		  case MULT_EXPR:
		  case TRUNC_DIV_EXPR:
		  case TRUNC_MOD_EXPR:
		  case FIX_TRUNC_EXPR:
		  case REALPART_EXPR:
		    if (gimple_store_p (use_stmt)
			&& is_gimple_assign (use_stmt)
			&& !gimple_has_volatile_ops (use_stmt)
			&& !stmt_ends_bb_p (use_stmt))
		      {
			tree lhs = gimple_assign_lhs (use_stmt);
			/* As multiply/division passes address of the lhs
			   to library function and that assumes it can extend
			   it to whole number of limbs, avoid merging those
			   with bit-field stores.  Don't allow it for
			   shifts etc. either, so that the bit-field store
			   handling doesn't have to be done everywhere.  */
			if (TREE_CODE (lhs) == COMPONENT_REF
			    && DECL_BIT_FIELD_TYPE (TREE_OPERAND (lhs, 1)))
			  break;
			continue;
		      }
		    break;
		  default:
		    break;
		  }
	    }

	  /* Also ignore uninitialized uses.  */
	  if (SSA_NAME_IS_DEFAULT_DEF (s)
	      && (!SSA_NAME_VAR (s) || VAR_P (SSA_NAME_VAR (s))))
	    continue;

	force_name:
	  if (!large_huge.m_names)
	    large_huge.m_names = BITMAP_ALLOC (NULL);
	  bitmap_set_bit (large_huge.m_names, SSA_NAME_VERSION (s));
	  if (has_single_use (s))
	    {
	      if (!large_huge.m_single_use_names)
		large_huge.m_single_use_names = BITMAP_ALLOC (NULL);
	      bitmap_set_bit (large_huge.m_single_use_names,
			      SSA_NAME_VERSION (s));
	    }
	  if (SSA_NAME_VAR (s)
	      && ((TREE_CODE (SSA_NAME_VAR (s)) == PARM_DECL
		   && SSA_NAME_IS_DEFAULT_DEF (s))
		  || TREE_CODE (SSA_NAME_VAR (s)) == RESULT_DECL))
	    has_large_huge_parm_result = true;
	  if (optimize
	      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (s)
	      && gimple_assign_load_p (SSA_NAME_DEF_STMT (s))
	      && !gimple_has_volatile_ops (SSA_NAME_DEF_STMT (s))
	      && !stmt_ends_bb_p (SSA_NAME_DEF_STMT (s)))
	    {
	      use_operand_p use_p;
	      imm_use_iterator iter;
	      bool optimizable_load = true;
	      FOR_EACH_IMM_USE_FAST (use_p, iter, s)
		{
		  gimple *use_stmt = USE_STMT (use_p);
		  if (is_gimple_debug (use_stmt))
		    continue;
		  if (gimple_code (use_stmt) == GIMPLE_PHI
		      || is_gimple_call (use_stmt)
		      || gimple_code (use_stmt) == GIMPLE_ASM
		      || (is_gimple_assign (use_stmt)
			  && (gimple_assign_rhs_code (use_stmt)
			      == COMPLEX_EXPR)))
		    {
		      optimizable_load = false;
		      break;
		    }
		}

	      ssa_op_iter oi;
	      FOR_EACH_SSA_USE_OPERAND (use_p, SSA_NAME_DEF_STMT (s),
					oi, SSA_OP_USE)
		{
		  tree s2 = USE_FROM_PTR (use_p);
		  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (s2))
		    {
		      optimizable_load = false;
		      break;
		    }
		}

	      if (optimizable_load && !stmt_ends_bb_p (SSA_NAME_DEF_STMT (s)))
		{
		  if (!large_huge.m_loads)
		    large_huge.m_loads = BITMAP_ALLOC (NULL);
		  bitmap_set_bit (large_huge.m_loads, SSA_NAME_VERSION (s));
		}
	    }
	}
      /* We need to also rewrite stores of large/huge _BitInt INTEGER_CSTs
	 into memory.  Such functions could have no large/huge SSA_NAMEs.  */
      else if (SSA_NAME_IS_VIRTUAL_OPERAND (s))
	{
	  gimple *g = SSA_NAME_DEF_STMT (s);
	  if (is_gimple_assign (g) && gimple_store_p (g))
	    {
	      tree t = gimple_assign_rhs1 (g);
	      if (TREE_CODE (TREE_TYPE (t)) == BITINT_TYPE
		  && bitint_precision_kind (TREE_TYPE (t)) >= bitint_prec_large)
		has_large_huge = true;
	    }
	}
    }

  if (large_huge.m_names || has_large_huge)
    {
      ret = TODO_update_ssa_only_virtuals | TODO_cleanup_cfg;
      calculate_dominance_info (CDI_DOMINATORS);
      if (optimize)
	enable_ranger (cfun);
      if (large_huge.m_loads)
	{
	  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (cfun);
	  entry->aux = NULL;
	  bitint_dom_walker (large_huge.m_names,
			     large_huge.m_loads).walk (entry);
	  bitmap_and_compl_into (large_huge.m_names, large_huge.m_loads);
	  clear_aux_for_blocks ();
	  BITMAP_FREE (large_huge.m_loads);
	}
      large_huge.m_limb_type = build_nonstandard_integer_type (limb_prec, 1);
      large_huge.m_limb_size
	= tree_to_uhwi (TYPE_SIZE_UNIT (large_huge.m_limb_type));
    }
  if (large_huge.m_names)
    {
      large_huge.m_map
	= init_var_map (num_ssa_names, NULL, large_huge.m_names);
      coalesce_ssa_name (large_huge.m_map);
      partition_view_normal (large_huge.m_map);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "After Coalescing:\n");
	  dump_var_map (dump_file, large_huge.m_map);
	}
      large_huge.m_vars
	= XCNEWVEC (tree, num_var_partitions (large_huge.m_map));
      bitmap_iterator bi;
      if (has_large_huge_parm_result)
	EXECUTE_IF_SET_IN_BITMAP (large_huge.m_names, 0, i, bi)
	  {
	    tree s = ssa_name (i);
	    if (SSA_NAME_VAR (s)
		&& ((TREE_CODE (SSA_NAME_VAR (s)) == PARM_DECL
		     && SSA_NAME_IS_DEFAULT_DEF (s))
		    || TREE_CODE (SSA_NAME_VAR (s)) == RESULT_DECL))
	      {
		int p = var_to_partition (large_huge.m_map, s);
		if (large_huge.m_vars[p] == NULL_TREE)
		  {
		    large_huge.m_vars[p] = SSA_NAME_VAR (s);
		    mark_addressable (SSA_NAME_VAR (s));
		  }
	      }
	  }
      tree atype = NULL_TREE;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Mapping SSA_NAMEs to decls:\n");
      EXECUTE_IF_SET_IN_BITMAP (large_huge.m_names, 0, i, bi)
	{
	  tree s = ssa_name (i);
	  int p = var_to_partition (large_huge.m_map, s);
	  if (large_huge.m_vars[p] == NULL_TREE)
	    {
	      if (atype == NULL_TREE
		  || !tree_int_cst_equal (TYPE_SIZE (atype),
					  TYPE_SIZE (TREE_TYPE (s))))
		{
		  unsigned HOST_WIDE_INT nelts
		    = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (s))) / limb_prec;
		  atype = build_array_type_nelts (large_huge.m_limb_type,
						  nelts);
		}
	      large_huge.m_vars[p] = create_tmp_var (atype, "bitint");
	      mark_addressable (large_huge.m_vars[p]);
	    }
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      print_generic_expr (dump_file, s, TDF_SLIM);
	      fprintf (dump_file, " -> ");
	      print_generic_expr (dump_file, large_huge.m_vars[p], TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    {
      gimple_stmt_iterator prev;
      for (gimple_stmt_iterator gsi = gsi_last_bb (bb); !gsi_end_p (gsi);
	   gsi = prev)
	{
	  prev = gsi;
	  gsi_prev (&prev);
	  ssa_op_iter iter;
	  gimple *stmt = gsi_stmt (gsi);
	  if (is_gimple_debug (stmt))
	    continue;
	  bitint_prec_kind kind = bitint_prec_small;
	  tree t;
	  FOR_EACH_SSA_TREE_OPERAND (t, stmt, iter, SSA_OP_ALL_OPERANDS)
	    if (TREE_CODE (TREE_TYPE (t)) == BITINT_TYPE)
	      {
		bitint_prec_kind this_kind
		  = bitint_precision_kind (TREE_TYPE (t));
		kind = MAX (kind, this_kind);
	      }
	  if (is_gimple_assign (stmt) && gimple_store_p (stmt))
	    {
	      t = gimple_assign_rhs1 (stmt);
	      if (TREE_CODE (TREE_TYPE (t)) == BITINT_TYPE)
		{
		  bitint_prec_kind this_kind
		    = bitint_precision_kind (TREE_TYPE (t));
		  kind = MAX (kind, this_kind);
		}
	    }
	  if (is_gimple_assign (stmt)
	      && gimple_assign_rhs_code (stmt) == FLOAT_EXPR)
	    {
	      t = gimple_assign_rhs1 (stmt);
	      if (TREE_CODE (TREE_TYPE (t)) == BITINT_TYPE
		  && TREE_CODE (t) == INTEGER_CST)
		{
		  bitint_prec_kind this_kind
		    = bitint_precision_kind (TREE_TYPE (t));
		  kind = MAX (kind, this_kind);
		}
	    }
	  if (is_gimple_call (stmt))
	    {
	      t = gimple_call_lhs (stmt);
	      if (t && TREE_CODE (TREE_TYPE (t)) == COMPLEX_TYPE)
		{
		  bitint_prec_kind this_kind = arith_overflow_arg_kind (stmt);
		  kind = MAX (kind, this_kind);
		  if (TREE_CODE (TREE_TYPE (TREE_TYPE (t))) == BITINT_TYPE)
		    {
		      this_kind
			= bitint_precision_kind (TREE_TYPE (TREE_TYPE (t)));
		      kind = MAX (kind, this_kind);
		    }
		}
	    }
	  if (kind == bitint_prec_small)
	    continue;
	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_CALL:
	      /* For now.  We'll need to handle some internal functions and
		 perhaps some builtins.  */
	      if (kind == bitint_prec_middle)
		continue;
	      break;
	    case GIMPLE_ASM:
	      if (kind == bitint_prec_middle)
		continue;
	      break;
	    case GIMPLE_RETURN:
	      continue;
	    case GIMPLE_ASSIGN:
	      if (gimple_clobber_p (stmt))
		continue;
	      if (kind >= bitint_prec_large)
		break;
	      if (gimple_assign_single_p (stmt))
		/* No need to lower copies, loads or stores.  */
		continue;
	      if (gimple_assign_cast_p (stmt))
		{
		  tree lhs = gimple_assign_lhs (stmt);
		  tree rhs = gimple_assign_rhs1 (stmt);
		  if (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
		      && INTEGRAL_TYPE_P (TREE_TYPE (rhs))
		      && (TYPE_PRECISION (TREE_TYPE (lhs))
			  == TYPE_PRECISION (TREE_TYPE (rhs))))
		    /* No need to lower casts to same precision.  */
		    continue;
		}
	      break;
	    default:
	      break;
	    }

	  if (kind == bitint_prec_middle)
	    {
	      tree type = NULL_TREE;
	      /* Middle _BitInt(N) is rewritten to casts to INTEGER_TYPEs
		 with the same precision and back.  */
	      unsigned int nops = gimple_num_ops (stmt);
	      for (unsigned int i = is_gimple_assign (stmt) ? 1 : 0;
		   i < nops; ++i)
		if (tree op = gimple_op (stmt, i))
		  {
		    tree nop = maybe_cast_middle_bitint (&gsi, op, type);
		    if (nop != op)
		      gimple_set_op (stmt, i, nop);
		    else if (COMPARISON_CLASS_P (op))
		      {
			TREE_OPERAND (op, 0)
			  = maybe_cast_middle_bitint (&gsi,
						      TREE_OPERAND (op, 0),
						      type);
			TREE_OPERAND (op, 1)
			  = maybe_cast_middle_bitint (&gsi,
						      TREE_OPERAND (op, 1),
						      type);
		      }
		    else if (TREE_CODE (op) == CASE_LABEL_EXPR)
		      {
			CASE_LOW (op)
			  = maybe_cast_middle_bitint (&gsi, CASE_LOW (op),
						      type);
			CASE_HIGH (op)
			  = maybe_cast_middle_bitint (&gsi, CASE_HIGH (op),
						      type);
		      }
		  }
	      if (tree lhs = gimple_get_lhs (stmt))
		if (TREE_CODE (TREE_TYPE (lhs)) == BITINT_TYPE
		    && (bitint_precision_kind (TREE_TYPE (lhs))
			== bitint_prec_middle))
		  {
		    int prec = TYPE_PRECISION (TREE_TYPE (lhs));
		    int uns = TYPE_UNSIGNED (TREE_TYPE (lhs));
		    type = build_nonstandard_integer_type (prec, uns);
		    tree lhs2 = make_ssa_name (type);
		    gimple_set_lhs (stmt, lhs2);
		    gimple *g = gimple_build_assign (lhs, NOP_EXPR, lhs2);
		    if (stmt_ends_bb_p (stmt))
		      {
			edge e = find_fallthru_edge (gsi_bb (gsi)->succs);
			gsi_insert_on_edge (e, g);
			edge_insertions = true;
		      }
		    else
		      gsi_insert_after (&gsi, g, GSI_SAME_STMT);
		  }
	      update_stmt (stmt);
	      continue;
	    }

	  if (tree lhs = gimple_get_lhs (stmt))
	    if (TREE_CODE (lhs) == SSA_NAME)
	      {
		tree type = TREE_TYPE (lhs);
		if (TREE_CODE (type) == COMPLEX_TYPE)
		  type = TREE_TYPE (type);
		if (TREE_CODE (type) == BITINT_TYPE
		    && bitint_precision_kind (type) >= bitint_prec_large
		    && (large_huge.m_names == NULL
			|| !bitmap_bit_p (large_huge.m_names,
					  SSA_NAME_VERSION (lhs))))
		  continue;
	      }

	  large_huge.lower_stmt (stmt);
	}

      tree atype = NULL_TREE;
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  tree lhs = gimple_phi_result (phi);
	  if (TREE_CODE (TREE_TYPE (lhs)) != BITINT_TYPE
	      || bitint_precision_kind (TREE_TYPE (lhs)) < bitint_prec_large)
	    continue;
	  int p1 = var_to_partition (large_huge.m_map, lhs);
	  gcc_assert (large_huge.m_vars[p1] != NULL_TREE);
	  tree v1 = large_huge.m_vars[p1];
	  for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	    {
	      tree arg = gimple_phi_arg_def (phi, i);
	      edge e = gimple_phi_arg_edge (phi, i);
	      gimple *g;
	      switch (TREE_CODE (arg))
		{
		case INTEGER_CST:
		  if (integer_zerop (arg) && VAR_P (v1))
		    {
		      tree zero = build_zero_cst (TREE_TYPE (v1));
		      g = gimple_build_assign (v1, zero);
		      gsi_insert_on_edge (e, g);
		      edge_insertions = true;
		      break;
		    }
		  int ext;
		  unsigned int min_prec, prec, rem;
		  tree c;
		  prec = TYPE_PRECISION (TREE_TYPE (arg));
		  rem = prec % (2 * limb_prec);
		  min_prec = bitint_min_cst_precision (arg, ext);
		  if (min_prec > prec - rem - 2 * limb_prec
		      && min_prec > (unsigned) limb_prec)
		    /* Constant which has enough significant bits that it
		       isn't worth trying to save .rodata space by extending
		       from smaller number.  */
		    min_prec = prec;
		  else
		    min_prec = CEIL (min_prec, limb_prec) * limb_prec;
		  if (min_prec == 0)
		    c = NULL_TREE;
		  else if (min_prec == prec)
		    c = tree_output_constant_def (arg);
		  else if (min_prec == (unsigned) limb_prec)
		    c = fold_convert (large_huge.m_limb_type, arg);
		  else
		    {
		      tree ctype = build_bitint_type (min_prec, 1);
		      c = tree_output_constant_def (fold_convert (ctype, arg));
		    }
		  if (c)
		    {
		      if (VAR_P (v1) && min_prec == prec)
			{
			  tree v2 = build1 (VIEW_CONVERT_EXPR,
					    TREE_TYPE (v1), c);
			  g = gimple_build_assign (v1, v2);
			  gsi_insert_on_edge (e, g);
			  edge_insertions = true;
			  break;
			}
		      if (TREE_CODE (TREE_TYPE (c)) == INTEGER_TYPE)
			g = gimple_build_assign (build1 (VIEW_CONVERT_EXPR,
							 TREE_TYPE (c), v1),
						 c);
		      else
			{
			  unsigned HOST_WIDE_INT nelts
			    = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (c)))
			      / limb_prec;
			  tree vtype
			    = build_array_type_nelts (large_huge.m_limb_type,
						      nelts);
			  g = gimple_build_assign (build1 (VIEW_CONVERT_EXPR,
							   vtype, v1),
						   build1 (VIEW_CONVERT_EXPR,
							   vtype, c));
			}
		      gsi_insert_on_edge (e, g);
		    }
		  if (ext == 0)
		    {
		      unsigned HOST_WIDE_INT nelts
			= (tree_to_uhwi (TYPE_SIZE (TREE_TYPE (v1)))
			   - min_prec) / limb_prec;
		      tree vtype
			= build_array_type_nelts (large_huge.m_limb_type,
						  nelts);
		      tree ptype = build_pointer_type (TREE_TYPE (v1));
		      tree off;
		      if (c)
			off = fold_convert (ptype,
					    TYPE_SIZE_UNIT (TREE_TYPE (c)));
		      else
			off = build_zero_cst (ptype);
		      tree vd = build2 (MEM_REF, vtype,
					build_fold_addr_expr (v1), off);
		      g = gimple_build_assign (vd, build_zero_cst (vtype));
		    }
		  else
		    {
		      tree vd = v1;
		      if (c)
			{
			  tree ptype = build_pointer_type (TREE_TYPE (v1));
			  tree off
			    = fold_convert (ptype,
					    TYPE_SIZE_UNIT (TREE_TYPE (c)));
			  vd = build2 (MEM_REF, large_huge.m_limb_type,
				       build_fold_addr_expr (v1), off);
			}
		      vd = build_fold_addr_expr (vd);
		      unsigned HOST_WIDE_INT nbytes
			= tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (v1)));
		      if (c)
			nbytes
			  -= tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (c)));
		      tree fn = builtin_decl_implicit (BUILT_IN_MEMSET);
		      g = gimple_build_call (fn, 3, vd,
					     integer_minus_one_node,
					     build_int_cst (sizetype,
							    nbytes));
		    }
		  gsi_insert_on_edge (e, g);
		  edge_insertions = true;
		  break;
		default:
		  gcc_unreachable ();
		case SSA_NAME:
		  if (gimple_code (SSA_NAME_DEF_STMT (arg)) == GIMPLE_NOP)
		    {
		      if (large_huge.m_names == NULL
			  || !bitmap_bit_p (large_huge.m_names,
					    SSA_NAME_VERSION (arg)))
			continue;
		    }
		  int p2 = var_to_partition (large_huge.m_map, arg);
		  if (p1 == p2)
		    continue;
		  gcc_assert (large_huge.m_vars[p2] != NULL_TREE);
		  tree v2 = large_huge.m_vars[p2];
		  if (VAR_P (v1) && VAR_P (v2))
		    g = gimple_build_assign (v1, v2);
		  else if (VAR_P (v1))
		    g = gimple_build_assign (v1, build1 (VIEW_CONVERT_EXPR,
							 TREE_TYPE (v1), v2));
		  else if (VAR_P (v2))
		    g = gimple_build_assign (build1 (VIEW_CONVERT_EXPR,
						     TREE_TYPE (v2), v1), v2);
		  else
		    {
		      if (atype == NULL_TREE
			  || !tree_int_cst_equal (TYPE_SIZE (atype),
						  TYPE_SIZE (TREE_TYPE (lhs))))
			{
			  unsigned HOST_WIDE_INT nelts
			    = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (lhs)))
			      / limb_prec;
			  atype
			    = build_array_type_nelts (large_huge.m_limb_type,
						      nelts);
			}
		      g = gimple_build_assign (build1 (VIEW_CONVERT_EXPR,
						       atype, v1),
					       build1 (VIEW_CONVERT_EXPR,
						       atype, v2));
		    }
		  gsi_insert_on_edge (e, g);
		  edge_insertions = true;
		  break;
		}
	    }
	}
    }

  if (large_huge.m_names || has_large_huge)
    {
      gimple *nop = NULL;
      for (i = 0; i < num_ssa_names; ++i)
	{
	  tree s = ssa_name (i);
	  if (s == NULL_TREE)
	    continue;
	  tree type = TREE_TYPE (s);
	  if (TREE_CODE (type) == COMPLEX_TYPE)
	    type = TREE_TYPE (type);
	  if (TREE_CODE (type) == BITINT_TYPE
	      && bitint_precision_kind (type) >= bitint_prec_large)
	    {
	      if (large_huge.m_preserved
		  && bitmap_bit_p (large_huge.m_preserved,
				   SSA_NAME_VERSION (s)))
		continue;
	      gimple *g = SSA_NAME_DEF_STMT (s);
	      if (gimple_code (g) == GIMPLE_NOP)
		{
		  if (SSA_NAME_VAR (s))
		    set_ssa_default_def (cfun, SSA_NAME_VAR (s), NULL_TREE);
		  release_ssa_name (s);
		  continue;
		}
	      if (gimple_bb (g) == NULL)
		{
		  release_ssa_name (s);
		  continue;
		}
	      if (gimple_code (g) != GIMPLE_ASM)
		{
		  gimple_stmt_iterator gsi = gsi_for_stmt (g);
		  bool save_vta = flag_var_tracking_assignments;
		  flag_var_tracking_assignments = false;
		  gsi_remove (&gsi, true);
		  flag_var_tracking_assignments = save_vta;
		}
	      if (nop == NULL)
		nop = gimple_build_nop ();
	      SSA_NAME_DEF_STMT (s) = nop;
	      release_ssa_name (s);
	    }
	}
      if (optimize)
	disable_ranger (cfun);
    }

  if (edge_insertions)
    gsi_commit_edge_inserts ();

  /* Fix up arguments of ECF_RETURNS_TWICE calls.  Those were temporarily
     inserted before the call, but that is invalid IL, so move them to the
     right place and add corresponding PHIs.  */
  if (!large_huge.m_returns_twice_calls.is_empty ())
    {
      auto_vec<gimple *, 16> arg_stmts;
      while (!large_huge.m_returns_twice_calls.is_empty ())
	{
	  gimple *stmt = large_huge.m_returns_twice_calls.pop ();
	  gimple_stmt_iterator gsi = gsi_after_labels (gimple_bb (stmt));
	  while (gsi_stmt (gsi) != stmt)
	    {
	      if (is_gimple_debug (gsi_stmt (gsi)))
		gsi_next (&gsi);
	      else
		{
		  arg_stmts.safe_push (gsi_stmt (gsi));
		  gsi_remove (&gsi, false);
		}
	    }
	  gimple *g;
	  basic_block bb = NULL;
	  edge e = NULL, ead = NULL;
	  FOR_EACH_VEC_ELT (arg_stmts, i, g)
	    {
	      gsi_safe_insert_before (&gsi, g);
	      if (i == 0)
		{
		  bb = gimple_bb (stmt);
		  gcc_checking_assert (EDGE_COUNT (bb->preds) == 2);
		  e = EDGE_PRED (bb, 0);
		  ead = EDGE_PRED (bb, 1);
		  if ((ead->flags & EDGE_ABNORMAL) == 0)
		    std::swap (e, ead);
		  gcc_checking_assert ((e->flags & EDGE_ABNORMAL) == 0
				       && (ead->flags & EDGE_ABNORMAL));
		}
	      tree lhs = gimple_assign_lhs (g);
	      tree arg = lhs;
	      gphi *phi = create_phi_node (copy_ssa_name (arg), bb);
	      add_phi_arg (phi, arg, e, UNKNOWN_LOCATION);
	      tree var = create_tmp_reg (TREE_TYPE (arg));
	      suppress_warning (var, OPT_Wuninitialized);
	      arg = get_or_create_ssa_default_def (cfun, var);
	      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (arg) = 1;
	      add_phi_arg (phi, arg, ead, UNKNOWN_LOCATION);
	      arg = gimple_phi_result (phi);
	      SSA_NAME_OCCURS_IN_ABNORMAL_PHI (arg) = 1;
	      imm_use_iterator iter;
	      gimple *use_stmt;
	      FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
		{
		  if (use_stmt == phi)
		    continue;
		  gcc_checking_assert (use_stmt == stmt);
		  use_operand_p use_p;
		  FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		    SET_USE (use_p, arg);
		}
	    }
	  update_stmt (stmt);
	  arg_stmts.truncate (0);
	}
    }

  return ret;
}

namespace {

const pass_data pass_data_lower_bitint =
{
  GIMPLE_PASS, /* type */
  "bitintlower", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  PROP_gimple_lbitint, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_lower_bitint : public gimple_opt_pass
{
public:
  pass_lower_bitint (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_bitint, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_lower_bitint (m_ctxt); }
  unsigned int execute (function *) final override
  {
    return gimple_lower_bitint ();
  }

}; // class pass_lower_bitint

} // anon namespace

gimple_opt_pass *
make_pass_lower_bitint (gcc::context *ctxt)
{
  return new pass_lower_bitint (ctxt);
}


namespace {

const pass_data pass_data_lower_bitint_O0 =
{
  GIMPLE_PASS, /* type */
  "bitintlower0", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  PROP_gimple_lbitint, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_lower_bitint_O0 : public gimple_opt_pass
{
public:
  pass_lower_bitint_O0 (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_bitint_O0, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *fun) final override
    {
      /* With errors, normal optimization passes are not run.  If we don't
	 lower bitint operations at all, rtl expansion will abort.  */
      return !(fun->curr_properties & PROP_gimple_lbitint);
    }

  unsigned int execute (function *) final override
  {
    return gimple_lower_bitint ();
  }

}; // class pass_lower_bitint_O0

} // anon namespace

gimple_opt_pass *
make_pass_lower_bitint_O0 (gcc::context *ctxt)
{
  return new pass_lower_bitint_O0 (ctxt);
}
