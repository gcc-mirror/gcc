/* Pointer Bounds Checker optimization pass.
   Copyright (C) 2014-2016 Free Software Foundation, Inc.
   Contributed by Ilya Enkovich (ilya.enkovich@intel.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "gimple-iterator.h"
#include "tree-chkp.h"
#include "ipa-chkp.h"

enum check_type
{
  CHECK_LOWER_BOUND,
  CHECK_UPPER_BOUND
};

struct pol_item
{
  tree cst;
  tree var;
};

struct address_t
{
  vec<struct pol_item> pol;
};

/* Structure to hold check informtation.  */
struct check_info
{
  /* Type of the check.  */
  check_type type;
  /* Address used for the check.  */
  address_t addr;
  /* Bounds used for the check.  */
  tree bounds;
  /* Check statement.  Can be NULL for removed checks.  */
  gimple *stmt;
};

/* Structure to hold checks information for BB.  */
struct bb_checks
{
  vec<struct check_info, va_heap, vl_ptr> checks;
};

static void chkp_collect_value (tree ssa_name, address_t &res);

#define chkp_bndmk_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_BNDMK))
#define chkp_intersect_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_INTERSECT))
#define chkp_checkl_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_BNDCL))
#define chkp_checku_fndecl \
  (targetm.builtin_chkp_function (BUILT_IN_CHKP_BNDCU))

static vec<struct bb_checks, va_heap, vl_ptr> check_infos;

/* Comparator for pol_item structures I1 and I2 to be used
   to find items with equal var.  Also used for polynomial
   sorting.  */
static int
chkp_pol_item_compare (const void *i1, const void *i2)
{
  const struct pol_item *p1 = (const struct pol_item *)i1;
  const struct pol_item *p2 = (const struct pol_item *)i2;

  if (p1->var == p2->var)
    return 0;
  else if (p1->var > p2->var)
    return 1;
  else
    return -1;
}

/* Find polynomial item in ADDR with var equal to VAR
   and return its index.  Return -1 if item was not
   found.  */
static int
chkp_pol_find (address_t &addr, tree var)
{
  int left = 0;
  int right = addr.pol.length () - 1;
  int n;

  while (right >= left)
    {
      n = (left + right) / 2;

      if (addr.pol[n].var == var
	  || (var && addr.pol[n].var
	      && TREE_CODE (var) == ADDR_EXPR
	      && TREE_CODE (addr.pol[n].var) == ADDR_EXPR
	      && TREE_OPERAND (var, 0) == TREE_OPERAND (addr.pol[n].var, 0)))
	return n;
      else if (addr.pol[n].var > var)
	right = n - 1;
      else
	left = n + 1;
    }

  return -1;
}

/* Return constant CST extended to size type.  */
static tree
chkp_extend_const (tree cst)
{
  if (TYPE_PRECISION (TREE_TYPE (cst)) < TYPE_PRECISION (size_type_node))
    return build_int_cst_type (size_type_node, tree_to_shwi (cst));

  return cst;
}

/* Add polynomial item CST * VAR to ADDR.  */
static void
chkp_add_addr_item (address_t &addr, tree cst, tree var)
{
  int n = chkp_pol_find (addr, var);

  cst = chkp_extend_const (cst);

  if (n < 0)
    {
      struct pol_item item;
      item.cst = cst;
      item.var = var;

      addr.pol.safe_push (item);
      addr.pol.qsort (&chkp_pol_item_compare);
    }
  else
    {
      addr.pol[n].cst = fold_build2 (PLUS_EXPR, TREE_TYPE (addr.pol[n].cst),
				     addr.pol[n].cst, cst);
      if (TREE_CODE (addr.pol[n].cst) == INTEGER_CST
	  && integer_zerop (addr.pol[n].cst))
	addr.pol.ordered_remove (n);
    }
}

/* Subtract polynomial item CST * VAR from ADDR.  */
static void
chkp_sub_addr_item (address_t &addr, tree cst, tree var)
{
  int n = chkp_pol_find (addr, var);

  cst = chkp_extend_const (cst);

  if (n < 0)
    {
      struct pol_item item;
      item.cst = fold_build2 (MINUS_EXPR, TREE_TYPE (cst),
			      integer_zero_node, cst);
      item.var = var;

      addr.pol.safe_push (item);
      addr.pol.qsort (&chkp_pol_item_compare);
    }
  else
    {
      addr.pol[n].cst = fold_build2 (MINUS_EXPR, TREE_TYPE (addr.pol[n].cst),
				     addr.pol[n].cst, cst);
      if (TREE_CODE (addr.pol[n].cst) == INTEGER_CST
	  && integer_zerop (addr.pol[n].cst))
	addr.pol.ordered_remove (n);
    }
}

/* Add address DELTA to ADDR.  */
static void
chkp_add_addr_addr (address_t &addr, address_t &delta)
{
  unsigned int i;
  for (i = 0; i < delta.pol.length (); i++)
    chkp_add_addr_item (addr, delta.pol[i].cst, delta.pol[i].var);
}

/* Subtract address DELTA from ADDR.  */
static void
chkp_sub_addr_addr (address_t &addr, address_t &delta)
{
  unsigned int i;
  for (i = 0; i < delta.pol.length (); i++)
    chkp_sub_addr_item (addr, delta.pol[i].cst, delta.pol[i].var);
}

/* Mutiply address ADDR by integer constant MULT.  */
static void
chkp_mult_addr (address_t &addr, tree mult)
{
  unsigned int i;
  for (i = 0; i < addr.pol.length (); i++)
    addr.pol[i].cst = fold_build2 (MULT_EXPR, TREE_TYPE (addr.pol[i].cst),
				   addr.pol[i].cst, mult);
}

/* Return 1 if we may prove ADDR has a constant value with
   determined sign, which is put into *SIGN.  Otherwise
   return 0.  */
static bool
chkp_is_constant_addr (const address_t &addr, int *sign)
{
  *sign = 0;

  if (addr.pol.length () == 0)
    return true;
  else if (addr.pol.length () > 1)
    return false;
  else if (addr.pol[0].var)
    return false;
  else if (integer_zerop (addr.pol[0].cst))
    *sign = 0;
  else if  (tree_int_cst_sign_bit (addr.pol[0].cst))
    *sign = -1;
  else
    *sign = 1;

  return true;
}

/* Dump ADDR into dump_file.  */
static void
chkp_print_addr (const address_t &addr)
{
  unsigned int n = 0;
  for (n = 0; n < addr.pol.length (); n++)
    {
      if (n > 0)
	fprintf (dump_file, " + ");

      if (addr.pol[n].var == NULL_TREE)
	print_generic_expr (dump_file, addr.pol[n].cst, 0);
      else
	{
	  if (TREE_CODE (addr.pol[n].cst) != INTEGER_CST
	      || !integer_onep (addr.pol[n].cst))
	    {
	      print_generic_expr (dump_file, addr.pol[n].cst, 0);
	      fprintf (dump_file, " * ");
	    }
	  print_generic_expr (dump_file, addr.pol[n].var, 0);
	}
    }
}

/* Compute value of PTR and put it into address RES.
   PTR has to be ADDR_EXPR.  */
static void
chkp_collect_addr_value (tree ptr, address_t &res)
{
  tree obj = TREE_OPERAND (ptr, 0);
  address_t addr;

  switch (TREE_CODE (obj))
    {
    case INDIRECT_REF:
      chkp_collect_value (TREE_OPERAND (obj, 0), res);
      break;

    case MEM_REF:
      chkp_collect_value (TREE_OPERAND (obj, 0), res);
      addr.pol.create (0);
      chkp_collect_value (TREE_OPERAND (obj, 1), addr);
      chkp_add_addr_addr (res, addr);
      addr.pol.release ();
      break;

    case ARRAY_REF:
      chkp_collect_value (build_fold_addr_expr (TREE_OPERAND (obj, 0)), res);
      addr.pol.create (0);
      chkp_collect_value (TREE_OPERAND (obj, 1), addr);
      chkp_mult_addr (addr, array_ref_element_size (obj));
      chkp_add_addr_addr (res, addr);
      addr.pol.release ();
      break;

    case COMPONENT_REF:
      {
	tree str = TREE_OPERAND (obj, 0);
	tree field = TREE_OPERAND (obj, 1);
	chkp_collect_value (build_fold_addr_expr (str), res);
	addr.pol.create (0);
	chkp_collect_value (component_ref_field_offset (obj), addr);
	chkp_add_addr_addr (res, addr);
	addr.pol.release ();
	if (DECL_FIELD_BIT_OFFSET (field))
	  {
	    addr.pol.create (0);
	    chkp_collect_value (fold_build2 (TRUNC_DIV_EXPR, size_type_node,
					     DECL_FIELD_BIT_OFFSET (field),
					     size_int (BITS_PER_UNIT)),
			   addr);
	    chkp_add_addr_addr (res, addr);
	    addr.pol.release ();
	  }
      }
      break;

    default:
      chkp_add_addr_item (res, integer_one_node, ptr);
      break;
    }
}

/* Compute value of PTR and put it into address RES.  */
static void
chkp_collect_value (tree ptr, address_t &res)
{
  gimple *def_stmt;
  enum gimple_code code;
  enum tree_code rhs_code;
  address_t addr;
  tree rhs1;

  if (TREE_CODE (ptr) == INTEGER_CST)
    {
      chkp_add_addr_item (res, ptr, NULL);
      return;
    }
  else if (TREE_CODE (ptr) == ADDR_EXPR)
    {
      chkp_collect_addr_value (ptr, res);
      return;
    }
  else if (TREE_CODE (ptr) != SSA_NAME)
    {
      chkp_add_addr_item (res, integer_one_node, ptr);
      return;
    }

  /* Now we handle the case when polynomial is computed
     for SSA NAME.  */
  def_stmt = SSA_NAME_DEF_STMT (ptr);
  code = gimple_code (def_stmt);

  /* Currently we do not walk through statements other
     than assignment.  */
  if (code != GIMPLE_ASSIGN)
    {
      chkp_add_addr_item (res, integer_one_node, ptr);
      return;
    }

  rhs_code = gimple_assign_rhs_code (def_stmt);
  rhs1 = gimple_assign_rhs1 (def_stmt);

  switch (rhs_code)
    {
    case SSA_NAME:
    case INTEGER_CST:
    case ADDR_EXPR:
      chkp_collect_value (rhs1, res);
      break;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      chkp_collect_value (rhs1, res);
      addr.pol.create (0);
      chkp_collect_value (gimple_assign_rhs2 (def_stmt), addr);
      chkp_add_addr_addr (res, addr);
      addr.pol.release ();
      break;

    case MINUS_EXPR:
      chkp_collect_value (rhs1, res);
      addr.pol.create (0);
      chkp_collect_value (gimple_assign_rhs2 (def_stmt), addr);
      chkp_sub_addr_addr (res, addr);
      addr.pol.release ();
      break;

    case MULT_EXPR:
      if (TREE_CODE (rhs1) == SSA_NAME
	  && TREE_CODE (gimple_assign_rhs2 (def_stmt)) == INTEGER_CST)
	{
	  chkp_collect_value (rhs1, res);
	  chkp_mult_addr (res, gimple_assign_rhs2 (def_stmt));
	}
      else if (TREE_CODE (gimple_assign_rhs2 (def_stmt)) == SSA_NAME
	       && TREE_CODE (rhs1) == INTEGER_CST)
	{
	  chkp_collect_value (gimple_assign_rhs2 (def_stmt), res);
	  chkp_mult_addr (res, rhs1);
	}
      else
	chkp_add_addr_item (res, integer_one_node, ptr);
      break;

    default:
      chkp_add_addr_item (res, integer_one_node, ptr);
      break;
    }
}

/* Fill check_info structure *CI with information about
   check STMT.  */
static void
chkp_fill_check_info (gimple *stmt, struct check_info *ci)
{
  ci->addr.pol.create (0);
  ci->bounds = gimple_call_arg (stmt, 1);
  chkp_collect_value (gimple_call_arg (stmt, 0), ci->addr);
  ci->type = (gimple_call_fndecl (stmt) == chkp_checkl_fndecl
	     ? CHECK_LOWER_BOUND
	     : CHECK_UPPER_BOUND);
  ci->stmt = stmt;
}

/* Release structures holding check information
   for current function.  */
static void
chkp_release_check_info (void)
{
  unsigned int n, m;

  if (check_infos.exists ())
    {
      for (n = 0; n < check_infos.length (); n++)
	{
	  for (m = 0; m < check_infos[n].checks.length (); m++)
	    if (check_infos[n].checks[m].addr.pol.exists ())
	      check_infos[n].checks[m].addr.pol.release ();
	  check_infos[n].checks.release ();
	}
      check_infos.release ();
    }
}

/* Create structures to hold check information
   for current function.  */
static void
chkp_init_check_info (void)
{
  struct bb_checks empty_bbc;
  int n;

  empty_bbc.checks = vNULL;

  chkp_release_check_info ();

  check_infos.create (last_basic_block_for_fn (cfun));
  for (n = 0; n < last_basic_block_for_fn (cfun); n++)
    {
      check_infos.safe_push (empty_bbc);
      check_infos.last ().checks.create (0);
    }
}

/* Find all checks in current function and store info about them
   in check_infos.  */
static void
chkp_gather_checks_info (void)
{
  basic_block bb;
  gimple_stmt_iterator i;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Gathering information about checks...\n");

  chkp_init_check_info ();

  FOR_EACH_BB_FN (bb, cfun)
    {
      struct bb_checks *bbc = &check_infos[bb->index];

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Searching checks in BB%d...\n", bb->index);

      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
        {
	  gimple *stmt = gsi_stmt (i);

	  if (gimple_code (stmt) != GIMPLE_CALL)
	    continue;

	  if (gimple_call_fndecl (stmt) == chkp_checkl_fndecl
	      || gimple_call_fndecl (stmt) == chkp_checku_fndecl)
	    {
	      struct check_info ci;

	      chkp_fill_check_info (stmt, &ci);
	      bbc->checks.safe_push (ci);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Adding check information:\n");
		  fprintf (dump_file, "  bounds: ");
		  print_generic_expr (dump_file, ci.bounds, 0);
		  fprintf (dump_file, "\n  address: ");
		  chkp_print_addr (ci.addr);
		  fprintf (dump_file, "\n  check: ");
		  print_gimple_stmt (dump_file, stmt, 0, 0);
		}
	    }
	}
    }
}

/* Return 1 if check CI against BOUNDS always pass,
   -1 if check CI against BOUNDS always fails and
   0 if we cannot compute check result.  */
static int
chkp_get_check_result (struct check_info *ci, tree bounds)
{
  gimple *bnd_def;
  address_t bound_val;
  int sign, res = 0;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Trying to compute result of the check\n");
      fprintf (dump_file, "  check: ");
      print_gimple_stmt (dump_file, ci->stmt, 0, 0);
      fprintf (dump_file, "  address: ");
      chkp_print_addr (ci->addr);
      fprintf (dump_file, "\n  bounds: ");
      print_generic_expr (dump_file, bounds, 0);
      fprintf (dump_file, "\n");
    }

  if (TREE_CODE (bounds) != SSA_NAME)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  result: bounds tree code is not ssa_name\n");
      return 0;
    }

  bnd_def = SSA_NAME_DEF_STMT (bounds);
  /* Currently we handle cases when bounds are result of bndmk
     or loaded static bounds var.  */
  if (gimple_code (bnd_def) == GIMPLE_CALL
      && gimple_call_fndecl (bnd_def) == chkp_bndmk_fndecl)
    {
      bound_val.pol.create (0);
      chkp_collect_value (gimple_call_arg (bnd_def, 0), bound_val);
      if (ci->type == CHECK_UPPER_BOUND)
	{
	  address_t size_val;
	  size_val.pol.create (0);
	  chkp_collect_value (gimple_call_arg (bnd_def, 1), size_val);
	  chkp_add_addr_addr (bound_val, size_val);
	  size_val.pol.release ();
	  chkp_add_addr_item (bound_val, integer_minus_one_node, NULL);
	}
    }
  else if (gimple_code (bnd_def) == GIMPLE_ASSIGN
	   && gimple_assign_rhs1 (bnd_def) == chkp_get_zero_bounds_var ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  result: always pass with zero bounds\n");
      return 1;
    }
  else if (gimple_code (bnd_def) == GIMPLE_ASSIGN
	   && gimple_assign_rhs1 (bnd_def) == chkp_get_none_bounds_var ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  result: always fails with none bounds\n");
      return -1;
    }
  else if (gimple_code (bnd_def) == GIMPLE_ASSIGN
	   && TREE_CODE (gimple_assign_rhs1 (bnd_def)) == VAR_DECL)
    {
      tree bnd_var = gimple_assign_rhs1 (bnd_def);
      tree var;
      tree size;

      if (!DECL_INITIAL (bnd_var)
	  || DECL_INITIAL (bnd_var) == error_mark_node)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  result: cannot compute bounds\n");
	  return 0;
	}

      gcc_assert (TREE_CODE (DECL_INITIAL (bnd_var)) == ADDR_EXPR);
      var = TREE_OPERAND (DECL_INITIAL (bnd_var), 0);

      bound_val.pol.create (0);
      chkp_collect_value (DECL_INITIAL (bnd_var), bound_val);
      if (ci->type == CHECK_UPPER_BOUND)
	{
	  if (VAR_P (var))
	    {
	      if (DECL_SIZE (var)
		  && !chkp_variable_size_type (TREE_TYPE (var)))
		size = DECL_SIZE_UNIT (var);
	      else
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  result: cannot compute bounds\n");
		  return 0;
		}
	    }
	  else
	    {
	      gcc_assert (TREE_CODE (var) == STRING_CST);
	      size = build_int_cst (size_type_node,
				    TREE_STRING_LENGTH (var));
	    }

	  address_t size_val;
	  size_val.pol.create (0);
	  chkp_collect_value (size, size_val);
	  chkp_add_addr_addr (bound_val, size_val);
	  size_val.pol.release ();
	  chkp_add_addr_item (bound_val, integer_minus_one_node, NULL);
	}
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  result: cannot compute bounds\n");
      return 0;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  bound value: ");
      chkp_print_addr (bound_val);
      fprintf (dump_file, "\n");
    }

  chkp_sub_addr_addr (bound_val, ci->addr);

  if (!chkp_is_constant_addr (bound_val, &sign))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  result: cannot compute result\n");

      res = 0;
    }
  else if (sign == 0
	   || (ci->type == CHECK_UPPER_BOUND && sign > 0)
	   || (ci->type == CHECK_LOWER_BOUND && sign < 0))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  result: always pass\n");

      res = 1;
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  result: always fail\n");

      res = -1;
    }

  bound_val.pol.release ();

  return res;
}

/* Try to compare bounds value and address value
   used in the check CI.  If we can prove that check
   always pass then remove it.  */
static void
chkp_remove_check_if_pass (struct check_info *ci)
{
  int result = 0;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Trying to remove check: ");
      print_gimple_stmt (dump_file, ci->stmt, 0, 0);
    }

  result = chkp_get_check_result (ci, ci->bounds);

  if (result == 1)
    {
      gimple_stmt_iterator i = gsi_for_stmt (ci->stmt);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  action: delete check (always pass)\n");

      gsi_remove (&i, true);
      unlink_stmt_vdef (ci->stmt);
      release_defs (ci->stmt);
      ci->stmt = NULL;
    }
  else if (result == -1)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  action: keep check (always fail)\n");
      warning_at (gimple_location (ci->stmt), OPT_Wchkp,
		  "memory access check always fail");
    }
  else if (result == 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  action: keep check (cannot compute result)\n");
    }
}

/* For bounds used in CI check if bounds are produced by
   intersection and we may use outer bounds instead.  If
   transformation is possible then fix check statement and
   recompute its info.  */
static void
chkp_use_outer_bounds_if_possible (struct check_info *ci)
{
  gimple *bnd_def;
  tree bnd1, bnd2, bnd_res = NULL;
  int check_res1, check_res2;

  if (TREE_CODE (ci->bounds) != SSA_NAME)
    return;

  bnd_def = SSA_NAME_DEF_STMT (ci->bounds);
  if (gimple_code (bnd_def) != GIMPLE_CALL
      || gimple_call_fndecl (bnd_def) != chkp_intersect_fndecl)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Check if bounds intersection is redundant: \n");
      fprintf (dump_file, "  check: ");
      print_gimple_stmt (dump_file, ci->stmt, 0, 0);
      fprintf (dump_file, "  intersection: ");
      print_gimple_stmt (dump_file, bnd_def, 0, 0);
      fprintf (dump_file, "\n");
    }

  bnd1 = gimple_call_arg (bnd_def, 0);
  bnd2 = gimple_call_arg (bnd_def, 1);

  check_res1 = chkp_get_check_result (ci, bnd1);
  check_res2 = chkp_get_check_result (ci, bnd2);
  if (check_res1 == 1)
    bnd_res = bnd2;
  else if (check_res1 == -1)
    bnd_res = bnd1;
  else if (check_res2 == 1)
    bnd_res = bnd1;
  else if (check_res2 == -1)
    bnd_res = bnd2;

  if (bnd_res)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  action: use ");
	  print_generic_expr (dump_file, bnd2, 0);
	  fprintf (dump_file, " instead of ");
	  print_generic_expr (dump_file, ci->bounds, 0);
	  fprintf (dump_file, "\n");
	}

      ci->bounds = bnd_res;
      gimple_call_set_arg (ci->stmt, 1, bnd_res);
      update_stmt (ci->stmt);
      chkp_fill_check_info (ci->stmt, ci);
    }
}

/*  Try to find checks whose bounds were produced by intersection
    which does not affect check result.  In such check outer bounds
    are used instead.  It allows to remove excess intersections
    and helps to compare checks.  */
static void
chkp_remove_excess_intersections (void)
{
  basic_block bb;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Searching for redundant bounds intersections...\n");

  FOR_EACH_BB_FN (bb, cfun)
    {
      struct bb_checks *bbc = &check_infos[bb->index];
      unsigned int no;

      /* Iterate through all found checks in BB.  */
      for (no = 0; no < bbc->checks.length (); no++)
	if (bbc->checks[no].stmt)
	  chkp_use_outer_bounds_if_possible (&bbc->checks[no]);
    }
}

/*  Try to remove all checks which are known to alwyas pass.  */
static void
chkp_remove_constant_checks (void)
{
  basic_block bb;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Searching for redundant checks...\n");

  FOR_EACH_BB_FN (bb, cfun)
    {
      struct bb_checks *bbc = &check_infos[bb->index];
      unsigned int no;

      /* Iterate through all found checks in BB.  */
      for (no = 0; no < bbc->checks.length (); no++)
	if (bbc->checks[no].stmt)
	  chkp_remove_check_if_pass (&bbc->checks[no]);
    }
}

/* Return fast version of string function FNCODE.  */
static tree
chkp_get_nobnd_fndecl (enum built_in_function fncode)
{
  /* Check if we are allowed to use fast string functions.  */
  if (!flag_chkp_use_fast_string_functions)
    return NULL_TREE;

  tree fndecl = NULL_TREE;

  switch (fncode)
    {
    case BUILT_IN_MEMCPY_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMCPY_NOBND);
      break;

    case BUILT_IN_MEMPCPY_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMPCPY_NOBND);
      break;

    case BUILT_IN_MEMMOVE_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMMOVE_NOBND);
      break;

    case BUILT_IN_MEMSET_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMSET_NOBND);
      break;

    case BUILT_IN_CHKP_MEMCPY_NOCHK_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMCPY_NOBND_NOCHK);
      break;

    case BUILT_IN_CHKP_MEMPCPY_NOCHK_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMPCPY_NOBND_NOCHK);
      break;

    case BUILT_IN_CHKP_MEMMOVE_NOCHK_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMMOVE_NOBND_NOCHK);
      break;

    case BUILT_IN_CHKP_MEMSET_NOCHK_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMSET_NOBND_NOCHK);
      break;

    default:
      break;
    }

  if (fndecl)
    fndecl = chkp_maybe_clone_builtin_fndecl (fndecl);

  return fndecl;
}


/* Return no-check version of string function FNCODE.  */
static tree
chkp_get_nochk_fndecl (enum built_in_function fncode)
{
  /* Check if we are allowed to use fast string functions.  */
  if (!flag_chkp_use_nochk_string_functions)
    return NULL_TREE;

  tree fndecl = NULL_TREE;

  switch (fncode)
    {
    case BUILT_IN_MEMCPY_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMCPY_NOCHK);
      break;

    case BUILT_IN_MEMPCPY_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMPCPY_NOCHK);
      break;

    case BUILT_IN_MEMMOVE_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMMOVE_NOCHK);
      break;

    case BUILT_IN_MEMSET_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMSET_NOCHK);
      break;

    case BUILT_IN_CHKP_MEMCPY_NOBND_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMCPY_NOBND_NOCHK);
      break;

    case BUILT_IN_CHKP_MEMPCPY_NOBND_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMPCPY_NOBND_NOCHK);
      break;

    case BUILT_IN_CHKP_MEMMOVE_NOBND_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMMOVE_NOBND_NOCHK);
      break;

    case BUILT_IN_CHKP_MEMSET_NOBND_CHKP:
      fndecl = builtin_decl_implicit (BUILT_IN_CHKP_MEMSET_NOBND_NOCHK);
      break;

    default:
      break;
    }

  if (fndecl)
    fndecl = chkp_maybe_clone_builtin_fndecl (fndecl);

  return fndecl;
}

/* Find memcpy, mempcpy, memmove and memset calls, perform
   checks before call and then call no_chk version of
   functions.  We do it on O2 to enable inlining of these
   functions during expand.

   Also try to find memcpy, mempcpy, memmove and memset calls
   which are known to not write pointers to memory and use
   faster function versions for them.  */
static void
chkp_optimize_string_function_calls (void)
{
  basic_block bb;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Searching for replaceable string function calls...\n");

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator i;

      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
        {
	  gimple *stmt = gsi_stmt (i);
	  tree fndecl;

	  if (gimple_code (stmt) != GIMPLE_CALL
	      || !gimple_call_with_bounds_p (stmt))
	    continue;

	  fndecl = gimple_call_fndecl (stmt);

	  if (!fndecl || DECL_BUILT_IN_CLASS (fndecl) != BUILT_IN_NORMAL)
	    continue;

	  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMCPY_CHKP
	      || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMPCPY_CHKP
	      || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMMOVE_CHKP
	      || DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMSET_CHKP)
	    {
	      tree dst = gimple_call_arg (stmt, 0);
	      tree dst_bnd = gimple_call_arg (stmt, 1);
	      bool is_memset = DECL_FUNCTION_CODE (fndecl) == BUILT_IN_MEMSET_CHKP;
	      tree size = gimple_call_arg (stmt, is_memset ? 3 : 4);
	      tree fndecl_nochk;
	      gimple_stmt_iterator j;
	      basic_block check_bb;
	      address_t size_val;
	      int sign;
	      bool known;

	      /* We may replace call with corresponding __chkp_*_nobnd
		 call in case destination pointer base type is not
		 void or pointer.  */
	      if (POINTER_TYPE_P (TREE_TYPE (dst))
		  && !VOID_TYPE_P (TREE_TYPE (TREE_TYPE (dst)))
		  && !chkp_type_has_pointer (TREE_TYPE (TREE_TYPE (dst))))
		{
		  tree fndecl_nobnd
		    = chkp_get_nobnd_fndecl (DECL_FUNCTION_CODE (fndecl));

		  if (fndecl_nobnd)
		    fndecl = fndecl_nobnd;
		}

	      fndecl_nochk = chkp_get_nochk_fndecl (DECL_FUNCTION_CODE (fndecl));

	      if (fndecl_nochk)
		fndecl = fndecl_nochk;

	      if (fndecl != gimple_call_fndecl (stmt))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Replacing call: ");
		      print_gimple_stmt (dump_file, stmt, 0,
					 TDF_VOPS|TDF_MEMSYMS);
		    }

		  gimple_call_set_fndecl (stmt, fndecl);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "With a new call: ");
		      print_gimple_stmt (dump_file, stmt, 0,
					 TDF_VOPS|TDF_MEMSYMS);
		    }
		}

	      /* If there is no nochk version of function then
		 do nothing.  Otherwise insert checks before
		 the call.  */
	      if (!fndecl_nochk)
		continue;

	      /* If size passed to call is known and > 0
		 then we may insert checks unconditionally.  */
	      size_val.pol.create (0);
	      chkp_collect_value (size, size_val);
	      known = chkp_is_constant_addr (size_val, &sign);
	      size_val.pol.release ();

	      /* If we are not sure size is not zero then we have
		 to perform runtime check for size and perform
		 checks only when size is not zero.  */
	      if (!known)
		{
		  gimple *check = gimple_build_cond (NE_EXPR,
						     size,
						     size_zero_node,
						     NULL_TREE,
						     NULL_TREE);

		  /* Split block before string function call.  */
		  gsi_prev (&i);
		  check_bb = insert_cond_bb (bb, gsi_stmt (i), check);

		  /* Set position for checks.  */
		  j = gsi_last_bb (check_bb);

		  /* The block was splitted and therefore we
		     need to set iterator to its end.  */
		  i = gsi_last_bb (bb);
		}
	      /* If size is known to be zero then no checks
		 should be performed.  */
	      else if (!sign)
		continue;
	      else
		j = i;

	      size = size_binop (MINUS_EXPR, size, size_one_node);
	      if (!is_memset)
		{
		  tree src = gimple_call_arg (stmt, 2);
		  tree src_bnd = gimple_call_arg (stmt, 3);

		  chkp_check_mem_access (src, fold_build_pointer_plus (src, size),
					 src_bnd, j, gimple_location (stmt),
					 integer_zero_node);
		}

	      chkp_check_mem_access (dst, fold_build_pointer_plus (dst, size),
				     dst_bnd, j, gimple_location (stmt),
				     integer_one_node);

	    }
	}
    }
}

/* Intrumentation pass inserts most of bounds creation code
   in the header of the function.  We want to move bounds
   creation closer to bounds usage to reduce bounds lifetime.
   We also try to avoid bounds creation code on paths where
   bounds are not used.  */
static void
chkp_reduce_bounds_lifetime (void)
{
  basic_block bb = FALLTHRU_EDGE (ENTRY_BLOCK_PTR_FOR_FN (cfun))->dest;
  gimple_stmt_iterator i;

  for (i = gsi_start_bb (bb); !gsi_end_p (i); )
    {
      gimple *dom_use, *use_stmt, *stmt = gsi_stmt (i);
      basic_block dom_bb;
      ssa_op_iter iter;
      imm_use_iterator use_iter;
      use_operand_p use_p;
      tree op;
      bool want_move = false;
      bool deps = false;

      if (gimple_code (stmt) == GIMPLE_CALL
	  && gimple_call_fndecl (stmt) == chkp_bndmk_fndecl)
	want_move = true;

      if (gimple_code (stmt) == GIMPLE_ASSIGN
	  && POINTER_BOUNDS_P (gimple_assign_lhs (stmt))
	  && gimple_assign_rhs_code (stmt) == VAR_DECL)
	want_move = true;

      if (!want_move)
	{
	  gsi_next (&i);
	  continue;
	}

      /* Check we do not increase other values lifetime.  */
      FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, iter, SSA_OP_USE)
	{
	  op = USE_FROM_PTR (use_p);

	  if (TREE_CODE (op) == SSA_NAME
	      && gimple_code (SSA_NAME_DEF_STMT (op)) != GIMPLE_NOP)
	    {
	      deps = true;
	      break;
	    }
	}

      if (deps)
	{
	  gsi_next (&i);
	  continue;
	}

      /* Check all usages of bounds.  */
      if (gimple_code (stmt) == GIMPLE_CALL)
	op = gimple_call_lhs (stmt);
      else
	{
	  gcc_assert (gimple_code (stmt) == GIMPLE_ASSIGN);
	  op = gimple_assign_lhs (stmt);
	}

      dom_use = NULL;
      dom_bb = NULL;

      FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, op)
	{
	  if (is_gimple_debug (use_stmt))
	    continue;

	  if (dom_bb &&
	      dominated_by_p (CDI_DOMINATORS,
			      dom_bb, gimple_bb (use_stmt)))
	    {
	      dom_use = use_stmt;
	      dom_bb = NULL;
	    }
	  else if (dom_bb)
	    dom_bb = nearest_common_dominator (CDI_DOMINATORS, dom_bb,
					       gimple_bb (use_stmt));
	  else if (!dom_use)
	    dom_use = use_stmt;
	  else if (stmt_dominates_stmt_p (use_stmt, dom_use))
	    dom_use = use_stmt;
	  else if (!stmt_dominates_stmt_p (dom_use, use_stmt)
		   /* If dom_use and use_stmt are PHI nodes in one BB
		      then it is OK to keep any of them as dom_use.
		      stmt_dominates_stmt_p returns 0 for such
		      combination, so check it here manually.  */
		   && (gimple_code (dom_use) != GIMPLE_PHI
		       || gimple_code (use_stmt) != GIMPLE_PHI
		       || gimple_bb (use_stmt) != gimple_bb (dom_use))
		   )
	    {
	      dom_bb = nearest_common_dominator (CDI_DOMINATORS,
						 gimple_bb (use_stmt),
						 gimple_bb (dom_use));
	      dom_use = NULL;
	    }
	}

      /* In case there is a single use, just move bounds
	 creation to the use.  */
      if (dom_use || dom_bb)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Moving creation of ");
	      print_generic_expr (dump_file, op, 0);
	      fprintf (dump_file, " down to its use.\n");
	    }

	  if (dom_use && gimple_code (dom_use) == GIMPLE_PHI)
	    {
	      dom_bb = get_immediate_dominator (CDI_DOMINATORS,
						gimple_bb (dom_use));
	      dom_use = NULL;
	    }

	  if (dom_bb == bb
	      || (dom_use && gimple_bb (dom_use) == bb))
	    {
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Cannot move statement bacause there is no "
			     "suitable dominator block other than entry block.\n");

		  gsi_next (&i);
	    }
	  else
	    {
	      if (dom_bb)
		{
		  gimple_stmt_iterator last = gsi_last_bb (dom_bb);
		  if (!gsi_end_p (last) && stmt_ends_bb_p (gsi_stmt (last)))
		    gsi_move_before (&i, &last);
		  else
		    gsi_move_after (&i, &last);
		}
	      else
		{
		  gimple_stmt_iterator gsi = gsi_for_stmt (dom_use);
		  gsi_move_before (&i, &gsi);
		}

	      gimple_set_vdef (stmt, NULL_TREE);
	      gimple_set_vuse (stmt, NULL_TREE);
	      update_stmt (stmt);
	    }
	}
      else
	gsi_next (&i);
    }
}

/* Initilize checker optimization pass.  */
static void
chkp_opt_init (void)
{
  check_infos.create (0);

  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

  /* With LTO constant bounds vars may be not initialized by now.
     Get constant bounds vars to handle their assignments during
     optimizations.  */
  chkp_get_zero_bounds_var ();
  chkp_get_none_bounds_var ();
}

/* Finalise checker optimization  pass.  */
static void
chkp_opt_fini (void)
{
  chkp_fix_cfg ();

  free_dominance_info (CDI_POST_DOMINATORS);
}

/* Checker optimization pass function.  */
static unsigned int
chkp_opt_execute (void)
{
  chkp_opt_init();

  /* This optimization may introduce new checks
     and thus we put it before checks search.  */
  chkp_optimize_string_function_calls ();

  chkp_gather_checks_info ();

  chkp_remove_excess_intersections ();

  chkp_remove_constant_checks ();

  chkp_reduce_bounds_lifetime ();

  chkp_release_check_info ();

  chkp_opt_fini ();

  return 0;
}

/* Pass gate.  */
static bool
chkp_opt_gate (void)
{
  return chkp_function_instrumented_p (cfun->decl)
    && (flag_chkp_optimize > 0
	|| (flag_chkp_optimize == -1 && optimize > 0));
}

namespace {

const pass_data pass_data_chkp_opt =
{
  GIMPLE_PASS, /* type */
  "chkpopt", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa | PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_verify_il
  | TODO_update_ssa /* todo_flags_finish */
};

class pass_chkp_opt : public gimple_opt_pass
{
public:
  pass_chkp_opt (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_chkp_opt, ctxt)
  {}

  /* opt_pass methods: */
  virtual opt_pass * clone ()
    {
      return new pass_chkp_opt (m_ctxt);
    }

  virtual bool gate (function *)
    {
      return chkp_opt_gate ();
    }

  virtual unsigned int execute (function *)
    {
      return chkp_opt_execute ();
    }

}; // class pass_chkp_opt

} // anon namespace

gimple_opt_pass *
make_pass_chkp_opt (gcc::context *ctxt)
{
  return new pass_chkp_opt (ctxt);
}
