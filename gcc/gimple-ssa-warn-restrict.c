/* Pass to detect and issue warnings for violations of the restrict
   qualifier.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   Contributed by Martin Sebor <msebor@redhat.com>.

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
#include "tree.h"
#include "gimple.h"
#include "domwalk.h"
#include "tree-pass.h"
#include "builtins.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "gimple-ssa-warn-restrict.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "params.h"
#include "tree-cfg.h"
#include "tree-object-size.h"
#include "calls.h"
#include "cfgloop.h"
#include "intl.h"

namespace {

const pass_data pass_data_wrestrict = {
  GIMPLE_PASS,
  "wrestrict",
  OPTGROUP_NONE,
  TV_NONE,
  PROP_cfg, /* Properties_required.  */
  0,	    /* properties_provided.  */
  0,	    /* properties_destroyed.  */
  0,	    /* properties_start */
  0,	    /* properties_finish */
};

/* Pass to detect violations of strict aliasing requirements in calls
   to built-in string and raw memory functions.  */
class pass_wrestrict : public gimple_opt_pass
{
 public:
  pass_wrestrict (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_wrestrict, ctxt)
    { }

  opt_pass *clone () { return new pass_wrestrict (m_ctxt); }

  virtual bool gate (function *);
  virtual unsigned int execute (function *);
};

bool
pass_wrestrict::gate (function *fun ATTRIBUTE_UNUSED)
{
  return warn_array_bounds != 0 || warn_restrict != 0;
}

/* Class to walk the basic blocks of a function in dominator order.  */
class wrestrict_dom_walker : public dom_walker
{
 public:
  wrestrict_dom_walker () : dom_walker (CDI_DOMINATORS) {}

  edge before_dom_children (basic_block) FINAL OVERRIDE;
  bool handle_gimple_call (gimple_stmt_iterator *);

 private:
  void check_call (gcall *);
};

edge
wrestrict_dom_walker::before_dom_children (basic_block bb)
{
  /* Iterate over statements, looking for function calls.  */
  for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
       gsi_next (&si))
    {
      gimple *stmt = gsi_stmt (si);
      if (!is_gimple_call (stmt))
	continue;

      if (gcall *call = as_a <gcall *> (stmt))
	check_call (call);
    }

  return NULL;
}

/* Execute the pass for function FUN, walking in dominator order.  */

unsigned
pass_wrestrict::execute (function *fun)
{
  calculate_dominance_info (CDI_DOMINATORS);

  wrestrict_dom_walker walker;
  walker.walk (ENTRY_BLOCK_PTR_FOR_FN (fun));

  return 0;
}

/* Description of a memory reference by a built-in function.  This
   is similar to ao_ref but made especially suitable for -Wrestrict
   and not for optimization.  */
struct builtin_memref
{
  /* The original pointer argument to the built-in function.  */
  tree ptr;
  /* The referenced subobject or NULL if not available, and the base
     object of the memory reference or NULL.  */
  tree ref;
  tree base;

  /* The size of the BASE object, PTRDIFF_MAX if indeterminate,
     and negative until (possibly lazily) initialized.  */
  offset_int basesize;

  /* The non-negative offset of the referenced subobject.  Used to avoid
     warnings for (apparently) possibly but not definitively overlapping
     accesses to member arrays.  Negative when unknown/invalid.  */
  offset_int refoff;

  /* The offset range relative to the base.  */
  offset_int offrange[2];
  /* The size range of the access to this reference.  */
  offset_int sizrange[2];

  /* True for "bounded" string functions like strncat, and strncpy
     and their variants that specify either an exact or upper bound
     on the size of the accesses they perform.  For strncat both
     the source and destination references are bounded.  For strncpy
     only the destination reference is.  */
  bool strbounded_p;

  builtin_memref (tree, tree);

  tree offset_out_of_bounds (int, offset_int[2]) const;

private:

  /* Ctor helper to set or extend OFFRANGE based on argument.  */
  void extend_offset_range (tree);

  /*  Ctor helper to determine BASE and OFFRANGE from argument.  */
  void set_base_and_offset (tree);
};

/* Description of a memory access by a raw memory or string built-in
   function involving a pair of builtin_memref's.  */
class builtin_access
{
 public:
  /* Destination and source memory reference.  */
  builtin_memref* const dstref;
  builtin_memref* const srcref;
  /* The size range of the access.  It's the greater of the accesses
     to the two references.  */
  HOST_WIDE_INT sizrange[2];

  /* The minimum and maximum offset of an overlap of the access
     (if it does, in fact, overlap), and the size of the overlap.  */
  HOST_WIDE_INT ovloff[2];
  HOST_WIDE_INT ovlsiz[2];

  /* True to consider valid only accesses to the smallest subobject
     and false for raw memory functions.  */
  bool strict () const
  {
    return detect_overlap != &builtin_access::generic_overlap;
  }

  builtin_access (gcall *, builtin_memref &, builtin_memref &);

  /* Entry point to determine overlap.  */
  bool overlap ();

 private:
  /* Implementation functions used to determine overlap.  */
  bool generic_overlap ();
  bool strcat_overlap ();
  bool strcpy_overlap ();

  bool no_overlap ()
  {
    return false;
  }

  offset_int overlap_size (const offset_int [2], const offset_int[2],
			   offset_int [2]);

 private:
  /* Temporaries used to compute the final result.  */
  offset_int dstoff[2];
  offset_int srcoff[2];
  offset_int dstsiz[2];
  offset_int srcsiz[2];

  /* Pointer to a member function to call to determine overlap.  */
  bool (builtin_access::*detect_overlap) ();
};

/* Initialize a memory reference representation from a pointer EXPR and
   a size SIZE in bytes.  If SIZE is NULL_TREE then the size is assumed
   to be unknown.  */

builtin_memref::builtin_memref (tree expr, tree size)
: ptr (expr),
  ref (),
  base (),
  basesize (-1),
  refoff (HOST_WIDE_INT_MIN),
  offrange (),
  sizrange (),
  strbounded_p ()
{
  /* Unfortunately, wide_int default ctor is a no-op so array members
     of the type must be set individually.  */
  offrange[0] = offrange[1] = 0;
  sizrange[0] = sizrange[1] = 0;

  const offset_int maxobjsize = tree_to_shwi (max_object_size ());

  /* Find the BASE object or pointer referenced by EXPR and set
     the offset range OFFRANGE in the process.  */
  set_base_and_offset (expr);

  if (size)
    {
      tree range[2];
      /* Determine the size range, allowing for the result to be [0, 0]
	 for SIZE in the anti-range ~[0, N] where N >= PTRDIFF_MAX.  */
      get_size_range (size, range, true);
      sizrange[0] = wi::to_offset (range[0]);
      sizrange[1] = wi::to_offset (range[1]);
      /* get_size_range returns SIZE_MAX for the maximum size.
	 Constrain it to the real maximum of PTRDIFF_MAX.  */
      if (sizrange[1] > maxobjsize)
	sizrange[1] = maxobjsize;
    }
  else
    sizrange[1] = maxobjsize;

  if (!DECL_P (base))
    return;

  /* If the offset could be in the range of the referenced object
     constrain its bounds so neither exceeds those of the object.  */
  if (offrange[0] < 0 && offrange[1] > 0)
    offrange[0] = 0;

  offset_int maxoff = maxobjsize;
  tree basetype = TREE_TYPE (base);
  if (TREE_CODE (basetype) == ARRAY_TYPE
      && ref
      && array_at_struct_end_p (ref))
    ;   /* Use the maximum possible offset for last member arrays.  */
  else if (tree basesize = TYPE_SIZE_UNIT (basetype))
    if (TREE_CODE (basesize) == INTEGER_CST)
      /* Size could be non-constant for a variable-length type such
	 as a struct with a VLA member (a GCC extension).  */
      maxoff = wi::to_offset (basesize);

  if (offrange[0] >= 0)
    {
      if (offrange[1] < 0)
	offrange[1] = offrange[0] <= maxoff ? maxoff : maxobjsize;
      else if (offrange[0] <= maxoff && offrange[1] > maxoff)
	offrange[1] = maxoff;
    }
}

/* Ctor helper to set or extend OFFRANGE based on the OFFSET argument.  */

void
builtin_memref::extend_offset_range (tree offset)
{
  const offset_int maxobjsize = tree_to_shwi (max_object_size ());

  if (TREE_CODE (offset) == INTEGER_CST)
    {
      offset_int off = int_cst_value (offset);
      if (off != 0)
	{
	  offrange[0] += off;
	  offrange[1] += off;
	}
      return;
    }

  if (TREE_CODE (offset) == SSA_NAME)
    {
      wide_int min, max;
      value_range_type rng = get_range_info (offset, &min, &max);
      if (rng == VR_RANGE)
	{
	  offrange[0] += offset_int::from (min, SIGNED);
	  offrange[1] += offset_int::from (max, SIGNED);
	}
      else if (rng == VR_ANTI_RANGE)
	{
	  offrange[0] += offset_int::from (max + 1, SIGNED);
	  offrange[1] += offset_int::from (min - 1, SIGNED);
	}
      else
	{
	  gimple *stmt = SSA_NAME_DEF_STMT (offset);
	  tree type;
	  if (is_gimple_assign (stmt)
	      && gimple_assign_rhs_code (stmt) == NOP_EXPR
	      && (type = TREE_TYPE (gimple_assign_rhs1 (stmt)))
	      && INTEGRAL_TYPE_P (type))
	    {
	      /* Use the bounds of the type of the NOP_EXPR operand
		 even if it's signed.  The result doesn't trigger
		 warnings but makes their output more readable.  */
	      offrange[0] += wi::to_offset (TYPE_MIN_VALUE (type));
	      offrange[1] += wi::to_offset (TYPE_MAX_VALUE (type));
	    }
	  else
	    offrange[1] += maxobjsize;
	}
      return;
    }

  offrange[1] += maxobjsize;
}

/* Determines the base object or pointer of the reference EXPR
   and the offset range from the beginning of the base.  */

void
builtin_memref::set_base_and_offset (tree expr)
{
  const offset_int maxobjsize = tree_to_shwi (max_object_size ());

  if (TREE_CODE (expr) == SSA_NAME)
    {
      /* Try to tease the offset out of the pointer.  */
      gimple *stmt = SSA_NAME_DEF_STMT (expr);
      if (!base
	  && gimple_assign_single_p (stmt)
	  && gimple_assign_rhs_code (stmt) == ADDR_EXPR)
	expr = gimple_assign_rhs1 (stmt);
      else if (is_gimple_assign (stmt))
	{
	  tree_code code = gimple_assign_rhs_code (stmt);
	  if (code == NOP_EXPR)
	    {
	      tree rhs = gimple_assign_rhs1 (stmt);
	      if (POINTER_TYPE_P (TREE_TYPE (rhs)))
		expr = gimple_assign_rhs1 (stmt);
	      else
		{
		  base = expr;
		  return;
		}
	    }
	  else if (code == POINTER_PLUS_EXPR)
	    {
	      expr = gimple_assign_rhs1 (stmt);

	      tree offset = gimple_assign_rhs2 (stmt);
	      extend_offset_range (offset);
	    }
	  else
	    {
	      base = expr;
	      return;
	    }
	}
      else
	{
	  base = expr;
	  return;
	}
    }

  if (TREE_CODE (expr) == ADDR_EXPR)
    expr = TREE_OPERAND (expr, 0);

  /* Stash the reference for offset validation.  */
  ref = expr;

  poly_int64 bitsize, bitpos;
  tree var_off;
  machine_mode mode;
  int sign, reverse, vol;

  /* Determine the base object or pointer of the reference and
     the constant bit offset from the beginning of the base.
     If the offset has a non-constant component, it will be in
     VAR_OFF.  MODE, SIGN, REVERSE, and VOL are write only and
     unused here.  */
  base = get_inner_reference (expr, &bitsize, &bitpos, &var_off,
			      &mode, &sign, &reverse, &vol);

  /* get_inner_reference is not expected to return null.  */
  gcc_assert (base != NULL);

  poly_int64 bytepos = exact_div (bitpos, BITS_PER_UNIT);

  /* Convert the poly_int64 offset to offset_int.  The offset
     should be constant but be prepared for it not to be just in
     case.  */
  offset_int cstoff;
  if (bytepos.is_constant (&cstoff))
    {
      offrange[0] += cstoff;
      offrange[1] += cstoff;

      /* Besides the reference saved above, also stash the offset
	 for validation.  */
      if (TREE_CODE (expr) == COMPONENT_REF)
	refoff = cstoff;
    }
  else
    offrange[1] += maxobjsize;

  if (var_off)
    {
      if (TREE_CODE (var_off) == INTEGER_CST)
	{
	  cstoff = wi::to_offset (var_off);
	  offrange[0] += cstoff;
	  offrange[1] += cstoff;
	}
      else
	offrange[1] += maxobjsize;
    }

  if (TREE_CODE (base) == MEM_REF)
    {
      tree memrefoff = TREE_OPERAND (base, 1);
      extend_offset_range (memrefoff);
      base = TREE_OPERAND (base, 0);
    }

  if (TREE_CODE (base) == SSA_NAME)
    set_base_and_offset (base);
}

/* Return error_mark_node if the signed offset exceeds the bounds
   of the address space (PTRDIFF_MAX).  Otherwise, return either
   BASE or REF when the offset exceeds the bounds of the BASE or
   REF object, and set OOBOFF to the past-the-end offset formed
   by the reference, including its size.  When STRICT is non-zero
   use REF size, when available, otherwise use BASE size.  When
   STRICT is greater than 1, use the size of the last array member
   as the bound, otherwise treat such a member as a flexible array
   member.  Return NULL when the offset is in bounds.  */

tree
builtin_memref::offset_out_of_bounds (int strict, offset_int ooboff[2]) const
{
  const offset_int maxobjsize = tree_to_shwi (max_object_size ());

  /* A temporary, possibly adjusted, copy of the offset range.  */
  offset_int offrng[2] = { offrange[0], offrange[1] };

  if (DECL_P (base) && TREE_CODE (TREE_TYPE (base)) == ARRAY_TYPE)
    {
      /* Check for offset in an anti-range with a negative lower bound.
	 For such a range, consider only the non-negative subrange.  */
      if (offrng[1] < offrng[0] && offrng[1] < 0)
  	offrng[1] = maxobjsize;
    }

  /* Conservative offset of the last byte of the referenced object.  */
  offset_int endoff;

  /* The bounds need not be ordered.  Set HIB to use as the index
     of the larger of the bounds and LOB as the opposite.  */
  bool hib = wi::les_p (offrng[0], offrng[1]);
  bool lob = !hib;

  if (basesize < 0)
    {
      endoff = offrng[lob] + sizrange[0];

      /* For a reference through a pointer to an object of unknown size
	 all initial offsets are considered valid, positive as well as
	 negative, since the pointer itself can point past the beginning
	 of the object.  However, the sum of the lower bound of the offset
	 and that of the size must be less than or equal than PTRDIFF_MAX.  */
      if (endoff > maxobjsize)
	return error_mark_node;

      return NULL_TREE;
    }

  /* A reference to an object of known size must be within the bounds
     of the base object.  */
  if (offrng[hib] < 0 || offrng[lob] > basesize)
    return base;

  /* The extent of the reference must also be within the bounds of
     the base object (if known) or the maximum object size otherwise.  */
  endoff = wi::smax (offrng[lob], 0) + sizrange[0];
  if (endoff > maxobjsize)
    return error_mark_node;

  offset_int size = basesize;
  tree obj = base;

  if (strict
      && DECL_P (obj)
      && ref
      && refoff >= 0
      && TREE_CODE (ref) == COMPONENT_REF
      && (strict > 1
	  || !array_at_struct_end_p (ref)))
    {
      /* If the reference is to a member subobject, the offset must
	 be within the bounds of the subobject.  */
      tree field = TREE_OPERAND (ref, 1);
      tree type = TREE_TYPE (field);
      if (tree sz = TYPE_SIZE_UNIT (type))
	if (TREE_CODE (sz) == INTEGER_CST)
	  {
	    size = refoff + wi::to_offset (sz);
	    obj = ref;
	  }
    }

  if (endoff <= size)
    return NULL_TREE;

  /* Set the out-of-bounds offset range to be one greater than
     that delimited by the reference including its size.  */
  ooboff[lob] = size + 1;

  if (endoff > ooboff[lob])
    ooboff[hib] = endoff;
  else
    ooboff[hib] = wi::smax (offrng[lob], 0) + sizrange[1];

  return obj;
}

/* Create an association between the memory references DST and SRC
   for access by a call EXPR to a memory or string built-in funtion.  */

builtin_access::builtin_access (gcall *call, builtin_memref &dst,
				builtin_memref &src)
: dstref (&dst), srcref (&src), sizrange (), ovloff (), ovlsiz (),
  dstoff (), srcoff (), dstsiz (), srcsiz ()
{
  /* Zero out since the offset_int ctors invoked above are no-op.  */
  dstoff[0] = dstoff[1] = 0;
  srcoff[0] = srcoff[1] = 0;
  dstsiz[0] = dstsiz[1] = 0;
  srcsiz[0] = srcsiz[1] = 0;

  /* Object Size Type to use to determine the size of the destination
     and source objects.  Overridden below for raw memory functions.  */
  int ostype = 1;

  /* True when the size of one reference depends on the offset of
     itself or the other.  */
  bool depends_p = true;

  /* True when the size of the destination reference DSTREF has been
     determined from SRCREF and so needs to be adjusted by the latter's
     offset.  Only meaningful for bounded string functions like strncpy.  */
  bool dstadjust_p = false;

  /* The size argument number (depends on the built-in).  */
  unsigned sizeargno = 2;

  tree func = gimple_call_fndecl (call);
  switch (DECL_FUNCTION_CODE (func))
    {
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY:
    case BUILT_IN_MEMPCPY_CHK:
      ostype = 0;
      depends_p = false;
      detect_overlap = &builtin_access::generic_overlap;
      break;

    case BUILT_IN_MEMMOVE:
    case BUILT_IN_MEMMOVE_CHK:
      /* For memmove there is never any overlap to check for.  */
      ostype = 0;
      depends_p = false;
      detect_overlap = &builtin_access::no_overlap;
      break;

    case BUILT_IN_STPNCPY:
    case BUILT_IN_STPNCPY_CHK:
    case BUILT_IN_STRNCPY:
    case BUILT_IN_STRNCPY_CHK:
      dstref->strbounded_p = true;
      detect_overlap = &builtin_access::strcpy_overlap;
      break;

    case BUILT_IN_STPCPY:
    case BUILT_IN_STPCPY_CHK:
    case BUILT_IN_STRCPY:
    case BUILT_IN_STRCPY_CHK:
      detect_overlap = &builtin_access::strcpy_overlap;
      break;

    case BUILT_IN_STRCAT:
    case BUILT_IN_STRCAT_CHK:
      detect_overlap = &builtin_access::strcat_overlap;
      break;

    case BUILT_IN_STRNCAT:
    case BUILT_IN_STRNCAT_CHK:
      dstref->strbounded_p = true;
      srcref->strbounded_p = true;
      detect_overlap = &builtin_access::strcat_overlap;
      break;

    default:
      /* Handle other string functions here whose access may need
	 to be validated for in-bounds offsets and non-overlapping
	 copies.  */
      return;
    }

  const offset_int maxobjsize = tree_to_shwi (max_object_size ());

  /* Try to determine the size of the base object.  compute_objsize
     expects a pointer so create one if BASE is a non-pointer object.  */
  tree addr;
  if (dst.basesize < 0)
    {
      addr = dst.base;
      if (!POINTER_TYPE_P (TREE_TYPE (addr)))
	addr = build1 (ADDR_EXPR, (TREE_TYPE (addr)), addr);

      if (tree dstsize = compute_objsize (addr, ostype))
	dst.basesize = wi::to_offset (dstsize);
      else if (POINTER_TYPE_P (TREE_TYPE (addr)))
	dst.basesize = HOST_WIDE_INT_MIN;
      else
	dst.basesize = maxobjsize;
    }

  if (src.basesize < 0)
    {
      addr = src.base;
      if (!POINTER_TYPE_P (TREE_TYPE (addr)))
	addr = build1 (ADDR_EXPR, (TREE_TYPE (addr)), addr);

      if (tree srcsize = compute_objsize (addr, ostype))
	src.basesize = wi::to_offset (srcsize);
      else if (POINTER_TYPE_P (TREE_TYPE (addr)))
	src.basesize = HOST_WIDE_INT_MIN;
      else
	src.basesize = maxobjsize;
    }

  /* If there is no dependency between the references or the base
     objects of the two references aren't the same there's nothing
     else to do.  */
  if (depends_p && dstref->base != srcref->base)
    return;

  /* ...otherwise, make adjustments for references to the same object
     by string built-in functions to reflect the constraints imposed
     by the function.  */

  /* For bounded string functions determine the range of the bound
     on the access.  For others, the range stays unbounded.  */
  offset_int bounds[2] = { maxobjsize, maxobjsize };
  if (dstref->strbounded_p)
    {
      tree size = gimple_call_arg (call, sizeargno);
      tree range[2];
      if (get_size_range (size, range, true))
	{
	  bounds[0] = wi::to_offset (range[0]);
	  bounds[1] = wi::to_offset (range[1]);
	}

      /* If both references' size ranges are indeterminate use the last
	 (size) argument from the function call as a substitute.  This
	 may only be necessary for strncpy (but not for memcpy where
	 the size range would have been already determined this way).  */
      if (dstref->sizrange[0] == 0 && dstref->sizrange[1] == maxobjsize
	  && srcref->sizrange[0] == 0 && srcref->sizrange[1] == maxobjsize)
	{
	  dstref->sizrange[0] = bounds[0];
	  dstref->sizrange[1] = bounds[1];
	}
    }

  /* The size range of one reference involving the same base object
     can be determined from the size range of the other reference.
     This makes it possible to compute accurate offsets for warnings
     involving functions like strcpy where the length of just one of
     the two arguments is known (determined by tree-ssa-strlen).  */
  if (dstref->sizrange[0] == 0 && dstref->sizrange[1] == maxobjsize)
    {
      /* When the destination size is unknown set it to the size of
	 the source.  */
      dstref->sizrange[0] = srcref->sizrange[0];
      dstref->sizrange[1] = srcref->sizrange[1];
    }
  else if (srcref->sizrange[0] == 0 && srcref->sizrange[1] == maxobjsize)
    {
      /* When the source size is unknown set it to the size of
	 the destination.  */
      srcref->sizrange[0] = dstref->sizrange[0];
      srcref->sizrange[1] = dstref->sizrange[1];

      if (depends_p)
	{
	  if (dstref->strbounded_p)
	    {
	      /* Read access by strncpy is bounded.  */
	      if (bounds[0] < srcref->sizrange[0])
		srcref->sizrange[0] = bounds[0];
	      if (bounds[1] < srcref->sizrange[1])
		srcref->sizrange[1] = bounds[1];
	    }

	  /* For string functions, adjust the size range of the source
	     reference by the inverse boundaries of the offset (because
	     the higher the offset into the string the shorter its
	     length).  */
	  if (srcref->offrange[1] >= 0
	      && srcref->offrange[1] < srcref->sizrange[0])
	    srcref->sizrange[0] -= srcref->offrange[1];
	  else
	    srcref->sizrange[0] = 0;

	  if (srcref->offrange[0] > 0)
	    {
	      if (srcref->offrange[0] < srcref->sizrange[1])
		srcref->sizrange[1] -= srcref->offrange[0];
	      else
		srcref->sizrange[1] = 0;
	    }

	  dstadjust_p = true;
	}
    }

  if (detect_overlap == &builtin_access::generic_overlap)
    {
      if (dstref->strbounded_p)
	{
	  dstref->sizrange[0] = bounds[0];
	  dstref->sizrange[1] = bounds[1];

	  if (dstref->sizrange[0] < srcref->sizrange[0])
	    srcref->sizrange[0] = dstref->sizrange[0];

	  if (dstref->sizrange[1] < srcref->sizrange[1])
	    srcref->sizrange[1] = dstref->sizrange[1];
	}
    }
  else if (detect_overlap == &builtin_access::strcpy_overlap)
    {
      if (!dstref->strbounded_p)
	{
	  /* For strcpy, adjust the destination size range to match that
	     of the source computed above.  */
	  if (depends_p && dstadjust_p)
	    {
	      dstref->sizrange[0] = srcref->sizrange[0];
	      dstref->sizrange[1] = srcref->sizrange[1];
	    }
	}
    }

  if (dstref->strbounded_p)
    {
      /* For strncpy, adjust the destination size range to match that
	 of the source computed above.  */
      dstref->sizrange[0] = bounds[0];
      dstref->sizrange[1] = bounds[1];

      if (bounds[0] < srcref->sizrange[0])
	srcref->sizrange[0] = bounds[0];

      if (bounds[1] < srcref->sizrange[1])
	srcref->sizrange[1] = bounds[1];
    }
}

offset_int
builtin_access::overlap_size (const offset_int a[2], const offset_int b[2],
			      offset_int *off)
{
  const offset_int *p = a;
  const offset_int *q = b;

  /* Point P at the bigger of the two ranges and Q at the smaller.  */
  if (wi::lts_p (a[1] - a[0], b[1] - b[0]))
    {
      p = b;
      q = a;
    }

  if (p[0] < q[0])
    {
      if (p[1] < q[0])
	return 0;

      *off = q[0];
      return wi::smin (p[1], q[1]) - q[0];
    }

  if (q[1] < p[0])
    return 0;

  off[0] = p[0];
  return q[1] - p[0];
}

/* Return true if the bounded mempry (memcpy amd similar) or string function
   access (strncpy and similar) ACS overlaps.  */

bool
builtin_access::generic_overlap ()
{
  builtin_access &acs = *this;
  const builtin_memref *dstref = acs.dstref;
  const builtin_memref *srcref = acs.srcref;

  gcc_assert (dstref->base == srcref->base);

  const offset_int maxobjsize = tree_to_shwi (max_object_size ());

  offset_int maxsize = dstref->basesize < 0 ? maxobjsize : dstref->basesize;
  gcc_assert (maxsize <= maxobjsize);

  /* Adjust the larger bounds of the offsets (which may be the first
     element if the lower bound is larger than the upper bound) to
     make them valid for the smallest access (if possible) but no smaller
     than the smaller bounds.  */
  gcc_assert (wi::les_p (acs.dstoff[0], acs.dstoff[1]));

  if (maxsize < acs.dstoff[1] + acs.dstsiz[0])
    acs.dstoff[1] = maxsize - acs.dstsiz[0];
  if (acs.dstoff[1] < acs.dstoff[0])
    acs.dstoff[1] = acs.dstoff[0];

  gcc_assert (wi::les_p (acs.srcoff[0], acs.srcoff[1]));

  if (maxsize < acs.srcoff[1] + acs.srcsiz[0])
    acs.srcoff[1] = maxsize - acs.srcsiz[0];
  if (acs.srcoff[1] < acs.srcoff[0])
    acs.srcoff[1] = acs.srcoff[0];

  /* Determine the minimum and maximum space for the access given
     the offsets.  */
  offset_int space[2];
  space[0] = wi::abs (acs.dstoff[0] - acs.srcoff[0]);
  space[1] = space[0];

  offset_int d = wi::abs (acs.dstoff[0] - acs.srcoff[1]);
  if (acs.srcsiz[0] > 0)
    {
      if (d < space[0])
	space[0] = d;

      if (space[1] < d)
	space[1] = d;
    }
  else
    space[1] = acs.dstsiz[1];

  d = wi::abs (acs.dstoff[1] - acs.srcoff[0]);
  if (d < space[0])
    space[0] = d;

  if (space[1] < d)
    space[1] = d;

  /* Treat raw memory functions both of whose references are bounded
     as special and permit uncertain overlaps to go undetected.  For
     all kinds of constant offset and constant size accesses, if
     overlap isn't certain it is not possible.  */
  bool overlap_possible = space[0] < acs.dstsiz[1];
  if (!overlap_possible)
    return false;

  bool overlap_certain = space[1] < acs.dstsiz[0];

  /* True when the size of one reference depends on the offset of
     the other.  */
  bool depends_p = detect_overlap != &builtin_access::generic_overlap;

  if (!overlap_certain)
    {
      if (!dstref->strbounded_p && !depends_p)
	/* Memcpy only considers certain overlap.  */
	return false;

      /* There's no way to distinguish an access to the same member
	 of a structure from one to two distinct members of the same
	 structure.  Give up to avoid excessive false positives.  */
      tree basetype = TREE_TYPE (dstref->base);

      if (POINTER_TYPE_P (basetype))
	basetype = TREE_TYPE (basetype);
      else
	while (TREE_CODE (basetype) == ARRAY_TYPE)
	  basetype = TREE_TYPE (basetype);

      if (RECORD_OR_UNION_TYPE_P (basetype))
	return false;
    }

  /* True for stpcpy and strcpy.  */
  bool stxcpy_p = (!dstref->strbounded_p
		   && detect_overlap == &builtin_access::strcpy_overlap);

  if (dstref->refoff >= 0
      && srcref->refoff >= 0
      && dstref->refoff != srcref->refoff
      && (stxcpy_p || dstref->strbounded_p || srcref->strbounded_p))
    return false;

  offset_int siz[2] = { maxobjsize + 1, 0 };

  ovloff[0] = HOST_WIDE_INT_MAX;
  ovloff[1] = HOST_WIDE_INT_MIN;

  /* Adjustment to the lower bound of the offset of the overlap to
     account for a subset of unbounded string calls where the size
     of the destination string depends on the length of the source
     which in turn depends on the offset into it.  */
  bool sub1;

  if (stxcpy_p)
    {
      sub1 = acs.dstoff[0] <= acs.srcoff[0];

      /* Iterate over the extreme locations (on the horizontal axis formed
	 by their offsets) and sizes of two regions and find their smallest
	 and largest overlap and the corresponding offsets.  */
      for (unsigned i = 0; i != 2; ++i)
	{
	  const offset_int a[2] = {
	    acs.dstoff[i], acs.dstoff[i] + acs.dstsiz[!i]
	  };

	  const offset_int b[2] = {
	    acs.srcoff[i], acs.srcoff[i] + acs.srcsiz[!i]
	  };

	  offset_int off;
	  offset_int sz = overlap_size (a, b, &off);
	  if (sz < siz[0])
	    siz[0] = sz;

	  if (siz[1] <= sz)
	    siz[1] = sz;

	  if (sz != 0)
	    {
	      if (wi::lts_p (off, ovloff[0]))
		ovloff[0] = off.to_shwi ();
	      if (wi::lts_p (ovloff[1], off))
		ovloff[1] = off.to_shwi ();
	    }
	}
    }
  else
    {
      sub1 = !depends_p;

      /* Iterate over the extreme locations (on the horizontal axis
	 formed by their offsets) and sizes of two regions and find
	 their smallest and largest overlap and the corresponding
	 offsets.  */

      for (unsigned io = 0; io != 2; ++io)
	for (unsigned is = 0; is != 2; ++is)
	  {
	    const offset_int a[2] = {
	      acs.dstoff[io], acs.dstoff[io] + acs.dstsiz[is]
	    };

	    for (unsigned jo = 0; jo != 2; ++jo)
	      for (unsigned js = 0; js != 2; ++js)
		{
		  if (depends_p)
		    {
		      /* For st{p,r}ncpy the size of the source sequence
			 depends on the offset into it.  */
		      if (js)
			break;
		      js = !jo;
		    }

		  const offset_int b[2] = {
		    acs.srcoff[jo], acs.srcoff[jo] + acs.srcsiz[js]
		  };

		  offset_int off;
		  offset_int sz = overlap_size (a, b, &off);
		  if (sz < siz[0])
		    siz[0] = sz;

		  if (siz[1] <= sz)
		    siz[1] = sz;

		  if (sz != 0)
		    {
		      if (wi::lts_p (off, ovloff[0]))
			ovloff[0] = off.to_shwi ();
		      if (wi::lts_p (ovloff[1], off))
			ovloff[1] = off.to_shwi ();
		    }
		}
	  }
    }

  ovlsiz[0] = siz[0].to_shwi ();
  ovlsiz[1] = siz[1].to_shwi ();

  if (ovlsiz[0] == 0 && ovlsiz[1] > 1)
    ovloff[0] = ovloff[1] + ovlsiz[1] - 1 - sub1;

  return true;
}

/* Return true if the strcat-like access overlaps.  */

bool
builtin_access::strcat_overlap ()
{
  builtin_access &acs = *this;
  const builtin_memref *dstref = acs.dstref;
  const builtin_memref *srcref = acs.srcref;

  gcc_assert (dstref->base == srcref->base);

  const offset_int maxobjsize = tree_to_shwi (max_object_size ());

  gcc_assert (dstref->base && dstref->base == srcref->base);

  /* Adjust for strcat-like accesses.  */

  /* As a special case for strcat, set the DSTREF offsets to the length
     of the source string since the function starts writing at the first
     nul, and set the size to 1 for the length of the nul.  */
  acs.dstoff[0] += acs.dstsiz[0];
  acs.dstoff[1] += acs.dstsiz[1];

  bool strfunc_unknown_args = acs.dstsiz[0] == 0 && acs.dstsiz[1] != 0;

  /* The lower bound is zero when the size is unknown because then
     overlap is not certain.  */
  acs.dstsiz[0] = strfunc_unknown_args ? 0 : 1;
  acs.dstsiz[1] = 1;

  offset_int maxsize = dstref->basesize < 0 ? maxobjsize : dstref->basesize;
  gcc_assert (maxsize <= maxobjsize);

  /* For references to the same base object, determine if there's a pair
     of valid offsets into the two references such that access between
     them doesn't overlap.  Adjust both upper bounds to be valid for
     the smaller size (i.e., at most MAXSIZE - SIZE).  */

  if (maxsize < acs.dstoff[1] + acs.dstsiz[0])
    acs.dstoff[1] = maxsize - acs.dstsiz[0];

  if (maxsize < acs.srcoff[1] + acs.srcsiz[0])
    acs.srcoff[1] = maxsize - acs.srcsiz[0];

  /* Check to see if there's enough space for both accesses without
     overlap.  Determine the optimistic (maximum) amount of available
     space.  */
  offset_int space;
  if (acs.dstoff[0] <= acs.srcoff[0])
    {
      if (acs.dstoff[1] < acs.srcoff[1])
	space = acs.srcoff[1] + acs.srcsiz[0] - acs.dstoff[0];
      else
	space = acs.dstoff[1] + acs.dstsiz[0] - acs.srcoff[0];
    }
  else
    space = acs.dstoff[1] + acs.dstsiz[0] - acs.srcoff[0];

  /* Overlap is certain if the distance between the farthest offsets
     of the opposite accesses is less than the sum of the lower bounds
     of the sizes of the two accesses.  */
  bool overlap_certain = space < acs.dstsiz[0] + acs.srcsiz[0];

  /* For a constant-offset, constant size access, consider the largest
     distance between the offset bounds and the lower bound of the access
     size.  If the overlap isn't certain return success.  */
  if (!overlap_certain
      && acs.dstoff[0] == acs.dstoff[1]
      && acs.srcoff[0] == acs.srcoff[1]
      && acs.dstsiz[0] == acs.dstsiz[1]
      && acs.srcsiz[0] == acs.srcsiz[1])
    return false;

  /* Overlap is not certain but may be possible.  */

  offset_int access_min = acs.dstsiz[0] + acs.srcsiz[0];

  /* Determine the conservative (minimum) amount of space.  */
  space = wi::abs (acs.dstoff[0] - acs.srcoff[0]);
  offset_int d = wi::abs (acs.dstoff[0] - acs.srcoff[1]);
  if (d < space)
    space = d;
  d = wi::abs (acs.dstoff[1] - acs.srcoff[0]);
  if (d < space)
    space = d;

  /* For a strict test (used for strcpy and similar with unknown or
     variable bounds or sizes), consider the smallest distance between
     the offset bounds and either the upper bound of the access size
     if known, or the lower bound otherwise.  */
  if (access_min <= space && (access_min != 0 || !strfunc_unknown_args))
    return false;

  /* When strcat overlap is certain it is always a single byte:
     the terminating NUL, regardless of offsets and sizes.  When
     overlap is only possible its range is [0, 1].  */
  acs.ovlsiz[0] = dstref->sizrange[0] == dstref->sizrange[1] ? 1 : 0;
  acs.ovlsiz[1] = 1;

  offset_int endoff = dstref->offrange[0] + dstref->sizrange[0];
  if (endoff <= srcref->offrange[0])
    acs.ovloff[0] = wi::smin (maxobjsize, srcref->offrange[0]).to_shwi ();
  else
    acs.ovloff[0] = wi::smin (maxobjsize, endoff).to_shwi ();

  acs.sizrange[0] = wi::smax (wi::abs (endoff - srcref->offrange[0]) + 1,
			      srcref->sizrange[0]).to_shwi ();
  if (dstref->offrange[0] == dstref->offrange[1])
    {
      if (srcref->offrange[0] == srcref->offrange[1])
	acs.ovloff[1] = acs.ovloff[0];
      else
	acs.ovloff[1]
	  = wi::smin (maxobjsize,
		      srcref->offrange[1] + srcref->sizrange[1]).to_shwi ();
    }
  else
    acs.ovloff[1]
      = wi::smin (maxobjsize,
		  dstref->offrange[1] + dstref->sizrange[1]).to_shwi ();

  if (acs.sizrange[0] == 0)
    acs.sizrange[0] = 1;
  acs.sizrange[1] = wi::smax (acs.dstsiz[1], srcref->sizrange[1]).to_shwi ();
  return true;
}

/* Return true if the strcpy-like access overlaps.  */

bool
builtin_access::strcpy_overlap ()
{
  return generic_overlap ();
}


/* Return true if DSTREF and SRCREF describe accesses that either overlap
   one another or that, in order not to overlap, would imply that the size
   of the referenced object(s) exceeds the maximum size of an object.  Set
   Otherwise, if DSTREF and SRCREF do not definitely overlap (even though
   they may overlap in a way that's not apparent from the available data),
   return false.  */

bool
builtin_access::overlap ()
{
  builtin_access &acs = *this;

  const offset_int maxobjsize = tree_to_shwi (max_object_size ());

  acs.sizrange[0] = wi::smax (dstref->sizrange[0],
			      srcref->sizrange[0]).to_shwi ();
  acs.sizrange[1] = wi::smax (dstref->sizrange[1],
			      srcref->sizrange[1]).to_shwi ();

  /* Check to see if the two references refer to regions that are
     too large not to overlap in the address space (whose maximum
     size is PTRDIFF_MAX).  */
  offset_int size = dstref->sizrange[0] + srcref->sizrange[0];
  if (maxobjsize < size)
    {
      acs.ovloff[0] = (maxobjsize - dstref->sizrange[0]).to_shwi ();
      acs.ovlsiz[0] = (size - maxobjsize).to_shwi ();
      return true;
    }

  /* If both base objects aren't known return the maximum possible
     offset that would make them not overlap.  */
  if (!dstref->base || !srcref->base)
    return false;

  /* Set the access offsets.  */
  acs.dstoff[0] = dstref->offrange[0];
  acs.dstoff[1] = dstref->offrange[1];

  /* If the base object is an array adjust the bounds of the offset
     to be non-negative and within the bounds of the array if possible.  */
  if (dstref->base
      && TREE_CODE (TREE_TYPE (dstref->base)) == ARRAY_TYPE)
    {
      if (acs.dstoff[0] < 0 && acs.dstoff[1] >= 0)
	acs.dstoff[0] = 0;

      if (acs.dstoff[1] < acs.dstoff[0])
	{
	  if (tree size = TYPE_SIZE_UNIT (TREE_TYPE (dstref->base)))
	    acs.dstoff[1] = wi::umin (acs.dstoff[1], wi::to_offset (size));
	  else
	    acs.dstoff[1] = wi::umin (acs.dstoff[1], maxobjsize);
	}
    }

  acs.srcoff[0] = srcref->offrange[0];
  acs.srcoff[1] = srcref->offrange[1];

  if (srcref->base
      && TREE_CODE (TREE_TYPE (srcref->base)) == ARRAY_TYPE)
    {
      if (acs.srcoff[0] < 0 && acs.srcoff[1] >= 0)
	acs.srcoff[0] = 0;

      if (tree size = TYPE_SIZE_UNIT (TREE_TYPE (srcref->base)))
	acs.srcoff[1] = wi::umin (acs.srcoff[1], wi::to_offset (size));
      else if (acs.srcoff[1] < acs.srcoff[0])
	acs.srcoff[1] = wi::umin (acs.srcoff[1], maxobjsize);
    }

  /* When the upper bound of the offset is less than the lower bound
     the former is the result of a negative offset being represented
     as a large positive value or vice versa.  The resulting range is
     a union of two subranges: [MIN, UB] and [LB, MAX].  Since such
     a union is not representable using the current data structure
     replace it with the full range of offsets.  */
  if (acs.dstoff[1] < acs.dstoff[0])
    {
      acs.dstoff[0] = -maxobjsize - 1;
      acs.dstoff[1] = maxobjsize;
    }

  /* Validate the offset and size of each reference on its own first.
     This is independent of whether or not the base objects are the
     same.  Normally, this would have already been detected and
     diagnosed by -Warray-bounds, unless it has been disabled.  */
  offset_int maxoff = acs.dstoff[0] + dstref->sizrange[0];
  if (maxobjsize < maxoff)
    {
      acs.ovlsiz[0] = (maxoff - maxobjsize).to_shwi ();
      acs.ovloff[0] = acs.dstoff[0].to_shwi () - acs.ovlsiz[0];
      return true;
    }

  /* Repeat the same as above but for the source offsets.  */
  if (acs.srcoff[1] < acs.srcoff[0])
    {
      acs.srcoff[0] = -maxobjsize - 1;
      acs.srcoff[1] = maxobjsize;
    }

  maxoff = acs.srcoff[0] + srcref->sizrange[0];
  if (maxobjsize < maxoff)
    {
      acs.ovlsiz[0] = (maxoff - maxobjsize).to_shwi ();
      acs.ovlsiz[1] = (acs.srcoff[0] + srcref->sizrange[1]
		       - maxobjsize).to_shwi ();
      acs.ovloff[0] = acs.srcoff[0].to_shwi () - acs.ovlsiz[0];
      return true;
    }

  if (dstref->base != srcref->base)
    return false;

  acs.dstsiz[0] = dstref->sizrange[0];
  acs.dstsiz[1] = dstref->sizrange[1];

  acs.srcsiz[0] = srcref->sizrange[0];
  acs.srcsiz[1] = srcref->sizrange[1];

  /* Call the appropriate function to determine the overlap.  */
  if ((this->*detect_overlap) ())
    {
      if (!sizrange[1])
	{
	  /* Unless the access size range has already been set, do so here.  */
	  sizrange[0] = wi::smax (acs.dstsiz[0], srcref->sizrange[0]).to_shwi ();
	  sizrange[1] = wi::smax (acs.dstsiz[1], srcref->sizrange[1]).to_shwi ();
	}
      return true;
    }

  return false;
}

/* Attempt to detect and diagnose an overlapping copy in a call expression
   EXPR involving an an access ACS to a built-in memory or string function.
   Return true when one has been detected, false otherwise.  */

static bool
maybe_diag_overlap (location_t loc, gcall *call, builtin_access &acs)
{
  if (!acs.overlap ())
    return false;

  /* For convenience.  */
  const builtin_memref &dstref = *acs.dstref;
  const builtin_memref &srcref = *acs.srcref;

  /* Determine the range of offsets and sizes of the overlap if it
     exists and issue diagnostics.  */
  HOST_WIDE_INT *ovloff = acs.ovloff;
  HOST_WIDE_INT *ovlsiz = acs.ovlsiz;
  HOST_WIDE_INT *sizrange = acs.sizrange;

  tree func = gimple_call_fndecl (call);

  /* To avoid a combinatorial explosion of diagnostics format the offsets
     or their ranges as strings and use them in the warning calls below.  */
  char offstr[3][64];

  if (dstref.offrange[0] == dstref.offrange[1]
      || dstref.offrange[1] > HOST_WIDE_INT_MAX)
    sprintf (offstr[0], HOST_WIDE_INT_PRINT_DEC,
	     dstref.offrange[0].to_shwi ());
  else
    sprintf (offstr[0],
	     "[" HOST_WIDE_INT_PRINT_DEC ", " HOST_WIDE_INT_PRINT_DEC "]",
	     dstref.offrange[0].to_shwi (),
	     dstref.offrange[1].to_shwi ());

  if (srcref.offrange[0] == srcref.offrange[1]
      || srcref.offrange[1] > HOST_WIDE_INT_MAX)
    sprintf (offstr[1],
	     HOST_WIDE_INT_PRINT_DEC,
	     srcref.offrange[0].to_shwi ());
  else
    sprintf (offstr[1],
	     "[" HOST_WIDE_INT_PRINT_DEC ", " HOST_WIDE_INT_PRINT_DEC "]",
	     srcref.offrange[0].to_shwi (),
	     srcref.offrange[1].to_shwi ());

  if (ovloff[0] == ovloff[1] || !ovloff[1])
    sprintf (offstr[2], HOST_WIDE_INT_PRINT_DEC, ovloff[0]);
  else
    sprintf (offstr[2],
	     "[" HOST_WIDE_INT_PRINT_DEC ", " HOST_WIDE_INT_PRINT_DEC "]",
	     ovloff[0], ovloff[1]);

  const offset_int maxobjsize = tree_to_shwi (max_object_size ());
  bool must_overlap = ovlsiz[0] > 0;

  if (ovlsiz[1] == 0)
    ovlsiz[1] = ovlsiz[0];

  if (must_overlap)
    {
      /* Issue definitive "overlaps" diagnostic in this block.  */

      if (sizrange[0] == sizrange[1])
	{
	  if (ovlsiz[0] == ovlsiz[1])
	    warning_at (loc, OPT_Wrestrict,
			sizrange[0] == 1
			? (ovlsiz[0] == 1
			   ? G_("%G%qD accessing %wu byte at offsets %s "
				"and %s overlaps %wu byte at offset %s")
			   :  G_("%G%qD accessing %wu byte at offsets %s "
				 "and %s overlaps %wu bytes at offset "
				 "%s"))
			: (ovlsiz[0] == 1
			   ? G_("%G%qD accessing %wu bytes at offsets %s "
				"and %s overlaps %wu byte at offset %s")
			   : G_("%G%qD accessing %wu bytes at offsets %s "
				"and %s overlaps %wu bytes at offset "
				"%s")),
			call, func, sizrange[0],
			offstr[0], offstr[1], ovlsiz[0], offstr[2]);
	  else if (ovlsiz[1] >= 0 && ovlsiz[1] < maxobjsize.to_shwi ())
	    warning_n (loc, OPT_Wrestrict, sizrange[0],
		       "%G%qD accessing %wu byte at offsets %s "
		       "and %s overlaps between %wu and %wu bytes "
		       "at offset %s",
		       "%G%qD accessing %wu bytes at offsets %s "
		       "and %s overlaps between %wu and %wu bytes "
		       "at offset %s",
		       call, func, sizrange[0], offstr[0], offstr[1],
		       ovlsiz[0], ovlsiz[1], offstr[2]);
	  else
	    warning_n (loc, OPT_Wrestrict, sizrange[0],
		       "%G%qD accessing %wu byte at offsets %s and "
		       "%s overlaps %wu or more bytes at offset %s",
		       "%G%qD accessing %wu bytes at offsets %s and "
		       "%s overlaps %wu or more bytes at offset %s",
		       call, func, sizrange[0],
		       offstr[0], offstr[1], ovlsiz[0], offstr[2]);
	  return true;
	}

      if (sizrange[1] >= 0 && sizrange[1] < maxobjsize.to_shwi ())
	{
	  if (ovlsiz[0] == ovlsiz[1])
	    warning_n (loc, OPT_Wrestrict, ovlsiz[0],
		       "%G%qD accessing between %wu and %wu bytes "
		       "at offsets %s and %s overlaps %wu byte at "
		       "offset %s",
		       "%G%qD accessing between %wu and %wu bytes "
		       "at offsets %s and %s overlaps %wu bytes "
		       "at offset %s",
		       call, func, sizrange[0], sizrange[1],
		       offstr[0], offstr[1], ovlsiz[0], offstr[2]);
	  else if (ovlsiz[1] >= 0 && ovlsiz[1] < maxobjsize.to_shwi ())
	    warning_at (loc, OPT_Wrestrict,
			"%G%qD accessing between %wu and %wu bytes at "
			"offsets %s and %s overlaps between %wu and %wu "
			"bytes at offset %s",
			call, func, sizrange[0], sizrange[1],
			offstr[0], offstr[1], ovlsiz[0], ovlsiz[1],
			offstr[2]);
	  else
	    warning_at (loc, OPT_Wrestrict,
			"%G%qD accessing between %wu and %wu bytes at "
			"offsets %s and %s overlaps %wu or more bytes "
			"at offset %s",
			call, func, sizrange[0], sizrange[1],
			offstr[0], offstr[1], ovlsiz[0], offstr[2]);
	  return true;
	}

      if (ovlsiz[0] != ovlsiz[1])
	ovlsiz[1] = maxobjsize.to_shwi ();

      if (ovlsiz[0] == ovlsiz[1])
	warning_n (loc, OPT_Wrestrict, ovlsiz[0],
		   "%G%qD accessing %wu or more bytes at offsets "
		   "%s and %s overlaps %wu byte at offset %s",
		   "%G%qD accessing %wu or more bytes at offsets "
		   "%s and %s overlaps %wu bytes at offset %s",
		   call, func, sizrange[0], offstr[0], offstr[1],
		   ovlsiz[0], offstr[2]);
      else if (ovlsiz[1] >= 0 && ovlsiz[1] < maxobjsize.to_shwi ())
	warning_at (loc, OPT_Wrestrict,
		    "%G%qD accessing %wu or more bytes at offsets %s "
		    "and %s overlaps between %wu and %wu bytes "
		    "at offset %s",
		    call, func, sizrange[0], offstr[0], offstr[1],
		    ovlsiz[0], ovlsiz[1], offstr[2]);
      else
	warning_at (loc, OPT_Wrestrict,
		    "%G%qD accessing %wu or more bytes at offsets %s "
		    "and %s overlaps %wu or more bytes at offset %s",
		    call, func, sizrange[0], offstr[0], offstr[1],
		    ovlsiz[0], offstr[2]);
      return true;
    }

  /* Use more concise wording when one of the offsets is unbounded
     to avoid confusing the user with large and mostly meaningless
     numbers.  */
  bool open_range;
  if (DECL_P (dstref.base) && TREE_CODE (TREE_TYPE (dstref.base)) == ARRAY_TYPE)
    open_range = ((dstref.offrange[0] == 0
		   && dstref.offrange[1] == maxobjsize)
		  || (srcref.offrange[0] == 0
		      && srcref.offrange[1] == maxobjsize));
  else
    open_range = ((dstref.offrange[0] == -maxobjsize - 1
		   && dstref.offrange[1] == maxobjsize)
		  || (srcref.offrange[0] == -maxobjsize - 1
		      && srcref.offrange[1] == maxobjsize));

  if (sizrange[0] == sizrange[1] || sizrange[1] == 1)
    {
      if (ovlsiz[1] == 1)
	{
	  if (open_range)
	    warning_n (loc, OPT_Wrestrict, sizrange[1],
		       "%G%qD accessing %wu byte may overlap "
		       "%wu byte",
		       "%G%qD accessing %wu bytes may overlap "
		       "%wu byte",
		       call, func, sizrange[1], ovlsiz[1]);
	  else
	    warning_n (loc, OPT_Wrestrict, sizrange[1],
		       "%G%qD accessing %wu byte at offsets %s "
		       "and %s may overlap %wu byte at offset %s",
		       "%G%qD accessing %wu bytes at offsets %s "
		       "and %s may overlap %wu byte at offset %s",
		       call, func, sizrange[1], offstr[0], offstr[1],
		       ovlsiz[1], offstr[2]);
	  return true;
	}

      if (open_range)
	warning_n (loc, OPT_Wrestrict, sizrange[1],
		   "%G%qD accessing %wu byte may overlap "
		   "up to %wu bytes",
		   "%G%qD accessing %wu bytes may overlap "
		   "up to %wu bytes",
		   call, func, sizrange[1], ovlsiz[1]);
      else
	warning_n (loc, OPT_Wrestrict, sizrange[1],
		   "%G%qD accessing %wu byte at offsets %s and "
		   "%s may overlap up to %wu bytes at offset %s",
		   "%G%qD accessing %wu bytes at offsets %s and "
		   "%s may overlap up to %wu bytes at offset %s",
		   call, func, sizrange[1], offstr[0], offstr[1],
		   ovlsiz[1], offstr[2]);
      return true;
    }

  if (sizrange[1] >= 0 && sizrange[1] < maxobjsize.to_shwi ())
    {
      if (open_range)
	warning_n (loc, OPT_Wrestrict, ovlsiz[1],
		   "%G%qD accessing between %wu and %wu bytes "
		   "may overlap %wu byte",
		   "%G%qD accessing between %wu and %wu bytes "
		   "may overlap up to %wu bytes",
		   call, func, sizrange[0], sizrange[1], ovlsiz[1]);
      else
	warning_n (loc, OPT_Wrestrict, ovlsiz[1],
		   "%G%qD accessing between %wu and %wu bytes "
		   "at offsets %s and %s may overlap %wu byte "
		   "at offset %s",
		   "%G%qD accessing between %wu and %wu bytes "
		   "at offsets %s and %s may overlap up to %wu "
		   "bytes at offset %s",
		   call, func, sizrange[0], sizrange[1],
		   offstr[0], offstr[1], ovlsiz[1], offstr[2]);
      return true;
    }

  warning_n (loc, OPT_Wrestrict, ovlsiz[1],
	     "%G%qD accessing %wu or more bytes at offsets %s "
	     "and %s may overlap %wu byte at offset %s",
	     "%G%qD accessing %wu or more bytes at offsets %s "
	     "and %s may overlap up to %wu bytes at offset %s",
	     call, func, sizrange[0], offstr[0], offstr[1],
	     ovlsiz[1], offstr[2]);

  return true;
}

/* Validate REF offsets in an EXPRession passed as an argument to a CALL
   to a built-in function FUNC to make sure they are within the bounds
   of the referenced object if its size is known, or PTRDIFF_MAX otherwise.
   Both initial values of the offsets and their final value computed by
   the function by incrementing the initial value by the size are
   validated.  Return true if the offsets are not valid and a diagnostic
   has been issued.  */

static bool
maybe_diag_offset_bounds (location_t loc, gcall *call, tree func, int strict,
			  tree expr, const builtin_memref &ref)
{
  if (!warn_array_bounds)
    return false;

  offset_int ooboff[] = { ref.offrange[0], ref.offrange[1] };
  tree oobref = ref.offset_out_of_bounds (strict, ooboff);
  if (!oobref)
    return false;

  if (EXPR_HAS_LOCATION (expr))
    loc = EXPR_LOCATION (expr);

  loc = expansion_point_location_if_in_system_header (loc);

  char rangestr[2][64];
  if (ooboff[0] == ooboff[1]
      || (ooboff[0] != ref.offrange[0]
	  && ooboff[0].to_shwi () >= ooboff[1].to_shwi ()))
    sprintf (rangestr[0], "%lli", (long long) ooboff[0].to_shwi ());
  else
    sprintf (rangestr[0], "[%lli, %lli]",
	     (long long) ooboff[0].to_shwi (),
	     (long long) ooboff[1].to_shwi ());

  bool warned = false;

  if (oobref == error_mark_node)
    {
      if (ref.sizrange[0] == ref.sizrange[1])
	sprintf (rangestr[1], "%lli", (long long) ref.sizrange[0].to_shwi ());
      else
	sprintf (rangestr[1], "[%lli, %lli]",
		 (long long) ref.sizrange[0].to_shwi (),
		 (long long) ref.sizrange[1].to_shwi ());

      tree type;

      if (DECL_P (ref.base)
	  && TREE_CODE (type = TREE_TYPE (ref.base)) == ARRAY_TYPE)
	{
	  if (warning_at (loc, OPT_Warray_bounds,
			  "%G%qD pointer overflow between offset %s "
			  "and size %s accessing array %qD with type %qT",
			  call, func, rangestr[0], rangestr[1], ref.base, type))
	    {
	      inform (DECL_SOURCE_LOCATION (ref.base),
		      "array %qD declared here", ref.base);
	      warned = true;
	    }
	  else
	    warned = warning_at (loc, OPT_Warray_bounds,
				 "%G%qD pointer overflow between offset %s "
				 "and size %s",
				 call, func, rangestr[0], rangestr[1]);
	}
      else
	warned = warning_at (loc, OPT_Warray_bounds,
			     "%G%qD pointer overflow between offset %s "
			     "and size %s",
			     call, func, rangestr[0], rangestr[1]);
    }
  else if (oobref == ref.base)
    {
      const offset_int maxobjsize = tree_to_shwi (max_object_size ());

      /* True when the offset formed by an access to the reference
	 is out of bounds, rather than the initial offset wich is
	 in bounds.  This implies access past the end.  */
      bool form = ooboff[0] != ref.offrange[0];

      if (DECL_P (ref.base))
	{
	  if ((ref.basesize < maxobjsize
	       && warning_at (loc, OPT_Warray_bounds,
			      form
			      ? G_("%G%qD forming offset %s is out of "
				   "the bounds [0, %wu] of object %qD with "
				   "type %qT")
			      : G_("%G%qD offset %s is out of the bounds "
				   "[0, %wu] of object %qD with type %qT"),
			      call, func, rangestr[0], ref.basesize.to_uhwi (),
			      ref.base, TREE_TYPE (ref.base)))
	      || warning_at (loc, OPT_Warray_bounds,
			     form
			     ? G_("%G%qD forming offset %s is out of "
				  "the bounds of object %qD with type %qT")
			     : G_("%G%qD offset %s is out of the bounds "
				  "of object %qD with type %qT"),
			     call, func, rangestr[0],
			     ref.base, TREE_TYPE (ref.base)))
	    {
	      inform (DECL_SOURCE_LOCATION (ref.base),
		      "%qD declared here", ref.base);
	      warned = true;
	    }
	}
      else if (ref.basesize < maxobjsize)
	warned = warning_at (loc, OPT_Warray_bounds,
			     form
			     ? G_("%G%qD forming offset %s is out "
				  "of the bounds [0, %wu]")
			     : G_("%G%qD offset %s is out "
				  "of the bounds [0, %wu]"),
			     call, func, rangestr[0], ref.basesize.to_uhwi ());
      else
	warned = warning_at (loc, OPT_Warray_bounds,
			     form
			     ? G_("%G%qD forming offset %s is out of bounds")
			     : G_("%G%qD offset %s is out of bounds"),
			     call, func, rangestr[0]);
    }
  else if (TREE_CODE (ref.ref) == MEM_REF)
    {
      tree type = TREE_TYPE (TREE_OPERAND (ref.ref, 0));
      if (POINTER_TYPE_P (type))
	type = TREE_TYPE (type);
      type = TYPE_MAIN_VARIANT (type);

      warned = warning_at (loc, OPT_Warray_bounds,
			   "%G%qD offset %s from the object at %qE is out "
			   "of the bounds of %qT",
			   call, func, rangestr[0], ref.base, type);
    }
  else
    {
      tree type = TYPE_MAIN_VARIANT (TREE_TYPE (ref.ref));

      warned = warning_at (loc, OPT_Warray_bounds,
			   "%G%qD offset %s from the object at %qE is out "
			   "of the bounds of referenced subobject %qD with "
			   "type %qT at offset %wu",
			   call, func, rangestr[0], ref.base,
			   TREE_OPERAND (ref.ref, 1), type,
			   ref.refoff.to_uhwi ());
    }

  return warned;
}

/* Check a CALL statement for restrict-violations and issue warnings
   if/when appropriate.  */

void
wrestrict_dom_walker::check_call (gcall *call)
{
  /* Avoid checking the call if it has already been diagnosed for
     some reason.  */
  if (gimple_no_warning_p (call))
    return;

  tree func = gimple_call_fndecl (call);
  if (!func || DECL_BUILT_IN_CLASS (func) != BUILT_IN_NORMAL)
    return;

  /* Argument number to extract from the call (depends on the built-in
     and its kind).  */
  unsigned dst_idx = -1;
  unsigned src_idx = -1;
  unsigned bnd_idx = -1;

  /* Is this CALL to a string function (as opposed to one to a raw
     memory function).  */
  bool strfun = true;

  switch (DECL_FUNCTION_CODE (func))
    {
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY:
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMMOVE:
    case BUILT_IN_MEMMOVE_CHK:
      strfun = false;
      /* Fall through.  */

    case BUILT_IN_STPNCPY:
    case BUILT_IN_STPNCPY_CHK:
    case BUILT_IN_STRNCAT:
    case BUILT_IN_STRNCAT_CHK:
    case BUILT_IN_STRNCPY:
    case BUILT_IN_STRNCPY_CHK:
      dst_idx = 0;
      src_idx = 1;
      bnd_idx = 2;
      break;

    case BUILT_IN_STPCPY:
    case BUILT_IN_STPCPY_CHK:
    case BUILT_IN_STRCPY:
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STRCAT:
    case BUILT_IN_STRCAT_CHK:
      dst_idx = 0;
      src_idx = 1;
      break;

    default:
      /* Handle other string functions here whose access may need
	 to be validated for in-bounds offsets and non-overlapping
	 copies.  */
      return;
    }

  unsigned nargs = gimple_call_num_args (call);

  tree dst = dst_idx < nargs ? gimple_call_arg (call, dst_idx) : NULL_TREE;
  tree src = src_idx < nargs ? gimple_call_arg (call, src_idx) : NULL_TREE;
  tree dstwr = bnd_idx < nargs ? gimple_call_arg (call, bnd_idx) : NULL_TREE;

  /* For string functions with an unspecified or unknown bound,
     assume the size of the access is one.  */
  if (!dstwr && strfun)
    dstwr = size_one_node;

  /* DST and SRC can be null for a call with an insufficient number
     of arguments to a built-in function declared without a protype.  */
  if (!dst || !src)
    return;

  /* DST, SRC, or DSTWR can also have the wrong type in a call to
     a function declared without a prototype.  Avoid checking such
     invalid calls.  */
  if (TREE_CODE (TREE_TYPE (dst)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (src)) != POINTER_TYPE
      || (dstwr && !INTEGRAL_TYPE_P (TREE_TYPE (dstwr))))
    return;

  if (check_bounds_or_overlap (call, dst, src, dstwr, NULL_TREE))
    return;

  /* Avoid diagnosing the call again.  */
  gimple_set_no_warning (call, true);
}

} /* anonymous namespace */

/* Attempt to detect and diagnose invalid offset bounds and (except for
   memmove) overlapping copy in a call expression EXPR from SRC to DST
   and DSTSIZE and SRCSIZE bytes, respectively.  Both DSTSIZE and
   SRCSIZE may be NULL.  Return false when one or the other has been
   detected and diagnosed, true otherwise.  */

bool
check_bounds_or_overlap (gcall *call, tree dst, tree src, tree dstsize,
			 tree srcsize, bool bounds_only /* = false */)
{
  location_t loc = gimple_nonartificial_location (call);
  loc = expansion_point_location_if_in_system_header (loc);

  tree func = gimple_call_fndecl (call);

  builtin_memref dstref (dst, dstsize);
  builtin_memref srcref (src, srcsize);

  builtin_access acs (call, dstref, srcref);

  /* Set STRICT to the value of the -Warray-bounds=N argument for
     string functions or when N > 1.  */
  int strict = (acs.strict () || warn_array_bounds > 1 ? warn_array_bounds : 0);

  /* Validate offsets first to make sure they are within the bounds
     of the destination object if its size is known, or PTRDIFF_MAX
     otherwise.  */
  if (maybe_diag_offset_bounds (loc, call, func, strict, dst, dstref)
      || maybe_diag_offset_bounds (loc, call, func, strict, src, srcref))
    {
      gimple_set_no_warning (call, true);
      return false;
    }

  bool check_overlap
    = (warn_restrict
       && (bounds_only
	   || (DECL_FUNCTION_CODE (func) != BUILT_IN_MEMMOVE
	       && DECL_FUNCTION_CODE (func) != BUILT_IN_MEMMOVE_CHK)));

  if (!check_overlap)
    return true;

  if (operand_equal_p (dst, src, 0))
    {
      /* Issue -Wrestrict unless the pointers are null (those do
	 not point to objects and so do not indicate an overlap;
	 such calls could be the result of sanitization and jump
	 threading).  */
      if (!integer_zerop (dst) && !gimple_no_warning_p (call))
	{
	  warning_at (loc, OPT_Wrestrict,
		      "%G%qD source argument is the same as destination",
		      call, func);
	  gimple_set_no_warning (call, true);
	  return false;
	}

      return true;
    }

  /* Return false when overlap has been detected.  */
  if (maybe_diag_overlap (loc, call, acs))
    {
      gimple_set_no_warning (call, true);
      return false;
    }

  return true;
}

gimple_opt_pass *
make_pass_warn_restrict (gcc::context *ctxt)
{
  return new pass_wrestrict (ctxt);
}
