/* Analysis of polymorphic call context.
   Copyright (C) 2013-2015 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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
#include "tm.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "fold-const.h"
#include "print-tree.h"
#include "calls.h"
#include "hashtab.h"
#include "hard-reg-set.h"
#include "function.h"
#include "rtl.h"
#include "flags.h"
#include "statistics.h"
#include "real.h"
#include "fixed-value.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "emit-rtl.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"
#include "tree-pass.h"
#include "target.h"
#include "tree-pretty-print.h"
#include "predict.h"
#include "basic-block.h"
#include "hash-map.h"
#include "is-a.h"
#include "plugin-api.h"
#include "ipa-ref.h"
#include "cgraph.h"
#include "ipa-utils.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-inline.h"
#include "diagnostic.h"
#include "tree-dfa.h"
#include "demangle.h"
#include "dbgcnt.h"
#include "gimple-pretty-print.h"
#include "stor-layout.h"
#include "intl.h"
#include "data-streamer.h"
#include "lto-streamer.h"
#include "streamer-hooks.h"
#include "tree-ssa-operands.h"
#include "tree-into-ssa.h"

/* Return true when TYPE contains an polymorphic type and thus is interesting
   for devirtualization machinery.  */

static bool contains_type_p (tree, HOST_WIDE_INT, tree,
			     bool consider_placement_new = true,
			     bool consider_bases = true);

bool
contains_polymorphic_type_p (const_tree type)
{
  type = TYPE_MAIN_VARIANT (type);

  if (RECORD_OR_UNION_TYPE_P (type))
    {
      if (TYPE_BINFO (type)
          && polymorphic_type_binfo_p (TYPE_BINFO (type)))
	return true;
      for (tree fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	if (TREE_CODE (fld) == FIELD_DECL
	    && !DECL_ARTIFICIAL (fld)
	    && contains_polymorphic_type_p (TREE_TYPE (fld)))
	  return true;
      return false;
    }
  if (TREE_CODE (type) == ARRAY_TYPE)
    return contains_polymorphic_type_p (TREE_TYPE (type));
  return false;
}

/* Return true if it seems valid to use placement new to build EXPECTED_TYPE
   at possition CUR_OFFSET within TYPE.  

   POD can be changed to an instance of a polymorphic type by
   placement new.  Here we play safe and assume that any
   non-polymorphic type is POD.  */
bool
possible_placement_new (tree type, tree expected_type,
			HOST_WIDE_INT cur_offset)
{
  if (cur_offset < 0)
    return true;
  return ((TREE_CODE (type) != RECORD_TYPE
	   || !TYPE_BINFO (type)
	   || cur_offset >= POINTER_SIZE
	   || !polymorphic_type_binfo_p (TYPE_BINFO (type)))
	  && (!TYPE_SIZE (type)
	      || !tree_fits_shwi_p (TYPE_SIZE (type))
	      || (cur_offset
		  + (expected_type ? tree_to_uhwi (TYPE_SIZE (expected_type))
		     : POINTER_SIZE)
		  <= tree_to_uhwi (TYPE_SIZE (type)))));
}

/* THIS->OUTER_TYPE is a type of memory object where object of OTR_TYPE
   is contained at THIS->OFFSET.  Walk the memory representation of
   THIS->OUTER_TYPE and find the outermost class type that match
   OTR_TYPE or contain OTR_TYPE as a base.  Update THIS
   to represent it.

   If OTR_TYPE is NULL, just find outermost polymorphic type with
   virtual table present at possition OFFSET.

   For example when THIS represents type
   class A
     {
       int a;
       class B b;
     }
   and we look for type at offset sizeof(int), we end up with B and offset 0.
   If the same is produced by multiple inheritance, we end up with A and offset
   sizeof(int). 

   If we can not find corresponding class, give up by setting
   THIS->OUTER_TYPE to OTR_TYPE and THIS->OFFSET to NULL. 
   Return true when lookup was sucesful.

   When CONSIDER_PLACEMENT_NEW is false, reject contexts that may be made
   valid only via alocation of new polymorphic type inside by means
   of placement new.

   When CONSIDER_BASES is false, only look for actual fields, not base types
   of TYPE.  */

bool
ipa_polymorphic_call_context::restrict_to_inner_class (tree otr_type,
						       bool consider_placement_new,
						       bool consider_bases)
{
  tree type = outer_type;
  HOST_WIDE_INT cur_offset = offset;
  bool speculative = false;
  bool size_unknown = false;
  unsigned HOST_WIDE_INT otr_type_size = POINTER_SIZE;

  /* Update OUTER_TYPE to match EXPECTED_TYPE if it is not set.  */
  if (!outer_type)
    {
      clear_outer_type (otr_type);
      type = otr_type;
      cur_offset = 0;
    }
 /* See if OFFSET points inside OUTER_TYPE.  If it does not, we know
    that the context is either invalid, or the instance type must be
    derived from OUTER_TYPE.

    Because the instance type may contain field whose type is of OUTER_TYPE,
    we can not derive any effective information about it.

    TODO: In the case we know all derrived types, we can definitely do better
    here.  */
  else if (TYPE_SIZE (outer_type)
	   && tree_fits_shwi_p (TYPE_SIZE (outer_type))
	   && tree_to_shwi (TYPE_SIZE (outer_type)) >= 0
	   && tree_to_shwi (TYPE_SIZE (outer_type)) <= offset)
   {
     bool der = maybe_derived_type; /* clear_outer_type will reset it.  */
     bool dyn = dynamic;
     clear_outer_type (otr_type);
     type = otr_type;
     cur_offset = 0;

     /* If derived type is not allowed, we know that the context is invalid.
	For dynamic types, we really do not have information about
	size of the memory location.  It is possible that completely
	different type is stored after outer_type.  */
     if (!der && !dyn)
       {
	 clear_speculation ();
	 invalid = true;
	 return false;
       }
   }

  if (otr_type && TYPE_SIZE (otr_type)
      && tree_fits_shwi_p (TYPE_SIZE (otr_type)))
    otr_type_size = tree_to_uhwi (TYPE_SIZE (otr_type));

  if (!type || offset < 0)
    goto no_useful_type_info;

  /* Find the sub-object the constant actually refers to and mark whether it is
     an artificial one (as opposed to a user-defined one).

     This loop is performed twice; first time for outer_type and second time
     for speculative_outer_type.  The second run has SPECULATIVE set.  */
  while (true)
    {
      unsigned HOST_WIDE_INT pos, size;
      tree fld;

      /* If we do not know size of TYPE, we need to be more conservative
         about accepting cases where we can not find EXPECTED_TYPE.
	 Generally the types that do matter here are of constant size.
	 Size_unknown case should be very rare.  */
      if (TYPE_SIZE (type)
	  && tree_fits_shwi_p (TYPE_SIZE (type))
	  && tree_to_shwi (TYPE_SIZE (type)) >= 0)
	size_unknown = false;
      else
	size_unknown = true;

      /* On a match, just return what we found.  */
      if ((otr_type
	   && types_odr_comparable (type, otr_type)
	   && types_same_for_odr (type, otr_type))
	  || (!otr_type
	      && TREE_CODE (type) == RECORD_TYPE
	      && TYPE_BINFO (type)
	      && polymorphic_type_binfo_p (TYPE_BINFO (type))))
	{
	  if (speculative)
	    {
	      /* If we did not match the offset, just give up on speculation.  */
	      if (cur_offset != 0
		  /* Also check if speculation did not end up being same as
		     non-speculation.  */
		  || (types_must_be_same_for_odr (speculative_outer_type,
						  outer_type)
		      && (maybe_derived_type
			  == speculative_maybe_derived_type)))
		clear_speculation ();
	      return true;
	    }
	  else
	    {
	      /* If type is known to be final, do not worry about derived
		 types.  Testing it here may help us to avoid speculation.  */
	      if (otr_type && TREE_CODE (outer_type) == RECORD_TYPE
		  && (!in_lto_p || odr_type_p (outer_type))
		  && type_known_to_have_no_deriavations_p (outer_type))
		maybe_derived_type = false;

	      /* Type can not contain itself on an non-zero offset.  In that case
		 just give up.  Still accept the case where size is now known.
		 Either the second copy may appear past the end of type or within
		 the non-POD buffer located inside the variably sized type
		 itself.  */
	      if (cur_offset != 0)
		goto no_useful_type_info;
	      /* If we determined type precisely or we have no clue on
 		 speuclation, we are done.  */
	      if (!maybe_derived_type || !speculative_outer_type
		  || !speculation_consistent_p (speculative_outer_type,
					        speculative_offset,
					        speculative_maybe_derived_type,
						otr_type))
		{
		  clear_speculation ();
	          return true;
		}
	      /* Otherwise look into speculation now.  */
	      else
		{
		  speculative = true;
		  type = speculative_outer_type;
		  cur_offset = speculative_offset;
		  continue;
		}
	    }
	}

      /* Walk fields and find corresponding on at OFFSET.  */
      if (TREE_CODE (type) == RECORD_TYPE)
	{
	  for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	    {
	      if (TREE_CODE (fld) != FIELD_DECL)
		continue;

	      pos = int_bit_position (fld);
	      if (pos > (unsigned HOST_WIDE_INT)cur_offset)
		continue;

	      /* Do not consider vptr itself.  Not even for placement new.  */
	      if (!pos && DECL_ARTIFICIAL (fld)
		  && POINTER_TYPE_P (TREE_TYPE (fld))
		  && TYPE_BINFO (type)
		  && polymorphic_type_binfo_p (TYPE_BINFO (type)))
		continue;

	      if (!DECL_SIZE (fld) || !tree_fits_uhwi_p (DECL_SIZE (fld)))
		goto no_useful_type_info;
	      size = tree_to_uhwi (DECL_SIZE (fld));

	      /* We can always skip types smaller than pointer size:
		 those can not contain a virtual table pointer.

		 Disqualifying fields that are too small to fit OTR_TYPE
		 saves work needed to walk them for no benefit.
		 Because of the way the bases are packed into a class, the
		 field's size may be smaller than type size, so it needs
		 to be done with a care.  */
		
	      if (pos <= (unsigned HOST_WIDE_INT)cur_offset
		  && (pos + size) >= (unsigned HOST_WIDE_INT)cur_offset
				     + POINTER_SIZE
		  && (!otr_type
		      || !TYPE_SIZE (TREE_TYPE (fld))
		      || !tree_fits_shwi_p (TYPE_SIZE (TREE_TYPE (fld)))
		      || (pos + tree_to_uhwi (TYPE_SIZE (TREE_TYPE (fld))))
			  >= cur_offset + otr_type_size))
		break;
	    }

	  if (!fld)
	    goto no_useful_type_info;

	  type = TYPE_MAIN_VARIANT (TREE_TYPE (fld));
	  cur_offset -= pos;
	  /* DECL_ARTIFICIAL represents a basetype.  */
	  if (!DECL_ARTIFICIAL (fld))
	    {
	      if (!speculative)
		{
		  outer_type = type;
		  offset = cur_offset;
		  /* As soon as we se an field containing the type,
		     we know we are not looking for derivations.  */
		  maybe_derived_type = false;
		}
	      else
		{
		  speculative_outer_type = type;
		  speculative_offset = cur_offset;
		  speculative_maybe_derived_type = false;
		}
	    }
	  else if (!consider_bases)
	    goto no_useful_type_info;
	}
      else if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree subtype = TYPE_MAIN_VARIANT (TREE_TYPE (type));

	  /* Give up if we don't know array field size.
	     Also give up on non-polymorphic types as they are used
	     as buffers for placement new.  */
	  if (!TYPE_SIZE (subtype)
	      || !tree_fits_shwi_p (TYPE_SIZE (subtype))
	      || tree_to_shwi (TYPE_SIZE (subtype)) <= 0
	      || !contains_polymorphic_type_p (subtype))
	    goto no_useful_type_info;

	  HOST_WIDE_INT new_offset = cur_offset % tree_to_shwi (TYPE_SIZE (subtype));

	  /* We may see buffer for placement new.  In this case the expected type
	     can be bigger than the subtype.  */
	  if (TYPE_SIZE (subtype)
	      && (cur_offset + otr_type_size
		  > tree_to_uhwi (TYPE_SIZE (subtype))))
	    goto no_useful_type_info;

	  cur_offset = new_offset;
	  type = subtype;
	  if (!speculative)
	    {
	      outer_type = type;
	      offset = cur_offset;
	      maybe_derived_type = false;
	    }
	  else
	    {
	      speculative_outer_type = type;
	      speculative_offset = cur_offset;
	      speculative_maybe_derived_type = false;
	    }
	}
      /* Give up on anything else.  */
      else
	{
no_useful_type_info:
	  if (maybe_derived_type && !speculative
	      && TREE_CODE (outer_type) == RECORD_TYPE
	      && TREE_CODE (otr_type) == RECORD_TYPE
	      && TYPE_BINFO (otr_type)
	      && !offset
	      && get_binfo_at_offset (TYPE_BINFO (otr_type), 0, outer_type))
	    {
	      clear_outer_type (otr_type);
	      if (!speculative_outer_type
		  || !speculation_consistent_p (speculative_outer_type,
						speculative_offset,
					        speculative_maybe_derived_type,
						otr_type))
		clear_speculation ();
	      if (speculative_outer_type)
		{
		  speculative = true;
		  type = speculative_outer_type;
		  cur_offset = speculative_offset;
		}
	      else
		return true;
	    }
	  /* We found no way to embedd EXPECTED_TYPE in TYPE.
	     We still permit two special cases - placement new and
	     the case of variadic types containing themselves.  */
	  if (!speculative
	      && consider_placement_new
	      && (size_unknown || !type || maybe_derived_type
		  || possible_placement_new (type, otr_type, cur_offset)))
	    {
	      /* In these weird cases we want to accept the context.
		 In non-speculative run we have no useful outer_type info
		 (TODO: we may eventually want to record upper bound on the
		  type size that can be used to prune the walk),
		 but we still want to consider speculation that may
		 give useful info.  */
	      if (!speculative)
		{
		  clear_outer_type (otr_type);
		  if (!speculative_outer_type
		      || !speculation_consistent_p (speculative_outer_type,
						    speculative_offset,
						    speculative_maybe_derived_type,
						    otr_type))
		    clear_speculation ();
		  if (speculative_outer_type)
		    {
		      speculative = true;
		      type = speculative_outer_type;
		      cur_offset = speculative_offset;
		    }
		  else
		    return true;
		}
	      else
		{
		  clear_speculation ();
	          return true;
		}
	    }
	  else
	    {
	      clear_speculation ();
	      if (speculative)
		return true;
	      clear_outer_type (otr_type);
	      invalid = true; 
	      return false;
	    }
	}
    }
}

/* Return true if OUTER_TYPE contains OTR_TYPE at OFFSET.
   CONSIDER_PLACEMENT_NEW makes function to accept cases where OTR_TYPE can
   be built within OUTER_TYPE by means of placement new.  CONSIDER_BASES makes
   function to accept cases where OTR_TYPE appears as base of OUTER_TYPE or as
   base of one of fields of OUTER_TYPE.  */

static bool
contains_type_p (tree outer_type, HOST_WIDE_INT offset,
		 tree otr_type,
		 bool consider_placement_new,
		 bool consider_bases)
{
  ipa_polymorphic_call_context context;

  /* Check that type is within range.  */
  if (offset < 0)
    return false;

  /* PR ipa/71207
     As OUTER_TYPE can be a type which has a diamond virtual inheritance,
     it's not necessary that INNER_TYPE will fit within OUTER_TYPE with
     a given offset.  It can happen that INNER_TYPE also contains a base object,
     however it would point to the same instance in the OUTER_TYPE.  */

  context.offset = offset;
  context.outer_type = TYPE_MAIN_VARIANT (outer_type);
  context.maybe_derived_type = false;
  context.dynamic = false;
  return context.restrict_to_inner_class (otr_type, consider_placement_new,
					  consider_bases);
}


/* Return a FUNCTION_DECL if BLOCK represents a constructor or destructor.
   If CHECK_CLONES is true, also check for clones of ctor/dtors.  */

tree
inlined_polymorphic_ctor_dtor_block_p (tree block, bool check_clones)
{
  tree fn = block_ultimate_origin (block);
  if (fn == NULL || TREE_CODE (fn) != FUNCTION_DECL)
    return NULL_TREE;

  if (TREE_CODE (TREE_TYPE (fn)) != METHOD_TYPE
      || (!DECL_CXX_CONSTRUCTOR_P (fn) && !DECL_CXX_DESTRUCTOR_P (fn)))
    {
      if (!check_clones)
	return NULL_TREE;

      /* Watch for clones where we constant propagated the first
	 argument (pointer to the instance).  */
      fn = DECL_ABSTRACT_ORIGIN (fn);
      if (!fn
	  || TREE_CODE (TREE_TYPE (fn)) != METHOD_TYPE
	  || (!DECL_CXX_CONSTRUCTOR_P (fn) && !DECL_CXX_DESTRUCTOR_P (fn)))
	return NULL_TREE;
    }

  if (flags_from_decl_or_type (fn) & (ECF_PURE | ECF_CONST))
    return NULL_TREE;

  return fn;
}


/* We know that the instance is stored in variable or parameter
   (not dynamically allocated) and we want to disprove the fact
   that it may be in construction at invocation of CALL.

   BASE represents memory location where instance is stored.
   If BASE is NULL, it is assumed to be global memory.
   OUTER_TYPE is known type of the instance or NULL if not
   known.

   For the variable to be in construction we actually need to
   be in constructor of corresponding global variable or
   the inline stack of CALL must contain the constructor.
   Check this condition.  This check works safely only before
   IPA passes, because inline stacks may become out of date
   later.  */

bool
decl_maybe_in_construction_p (tree base, tree outer_type,
			      gimple call, tree function)
{
  if (outer_type)
    outer_type = TYPE_MAIN_VARIANT (outer_type);
  gcc_assert (!base || DECL_P (base));

  /* After inlining the code unification optimizations may invalidate
     inline stacks.  Also we need to give up on global variables after
     IPA, because addresses of these may have been propagated to their
     constructors.  */
  if (DECL_STRUCT_FUNCTION (function)->after_inlining)
    return true;

  /* Pure functions can not do any changes on the dynamic type;
     that require writting to memory.  */
  if ((!base || !auto_var_in_fn_p (base, function))
      && flags_from_decl_or_type (function) & (ECF_PURE | ECF_CONST))
    return false;

  bool check_clones = !base || is_global_var (base);
  for (tree block = gimple_block (call); block && TREE_CODE (block) == BLOCK;
       block = BLOCK_SUPERCONTEXT (block))
    if (tree fn = inlined_polymorphic_ctor_dtor_block_p (block, check_clones))
      {
	tree type = TYPE_MAIN_VARIANT (method_class_type (TREE_TYPE (fn)));

	if (!outer_type || !types_odr_comparable (type, outer_type))
	  {
	    if (TREE_CODE (type) == RECORD_TYPE
		&& TYPE_BINFO (type)
		&& polymorphic_type_binfo_p (TYPE_BINFO (type)))
	      return true;
	  }
 	else if (types_same_for_odr (type, outer_type))
	  return true;
      }

  if (!base || (TREE_CODE (base) == VAR_DECL && is_global_var (base)))
    {
      if (TREE_CODE (TREE_TYPE (function)) != METHOD_TYPE
	  || (!DECL_CXX_CONSTRUCTOR_P (function)
	      && !DECL_CXX_DESTRUCTOR_P (function)))
	{
	  if (!DECL_ABSTRACT_ORIGIN (function))
	    return false;
	  /* Watch for clones where we constant propagated the first
	     argument (pointer to the instance).  */
	  function = DECL_ABSTRACT_ORIGIN (function);
	  if (!function
	      || TREE_CODE (TREE_TYPE (function)) != METHOD_TYPE
	      || (!DECL_CXX_CONSTRUCTOR_P (function)
		  && !DECL_CXX_DESTRUCTOR_P (function)))
	    return false;
	}
      tree type = TYPE_MAIN_VARIANT (method_class_type (TREE_TYPE (function)));
      if (!outer_type || !types_odr_comparable (type, outer_type))
	{
	  if (TREE_CODE (type) == RECORD_TYPE
	      && TYPE_BINFO (type)
	      && polymorphic_type_binfo_p (TYPE_BINFO (type)))
	    return true;
	}
      else if (types_same_for_odr (type, outer_type))
	return true;
    }
  return false;
}

/* Dump human readable context to F.  If NEWLINE is true, it will be terminated
   by a newline.  */

void
ipa_polymorphic_call_context::dump (FILE *f, bool newline) const
{
  fprintf (f, "    ");
  if (invalid)
    fprintf (f, "Call is known to be undefined");
  else
    {
      if (useless_p ())
	fprintf (f, "nothing known");
      if (outer_type || offset)
	{
	  fprintf (f, "Outer type%s:", dynamic ? " (dynamic)":"");
	  print_generic_expr (f, outer_type, TDF_SLIM);
	  if (maybe_derived_type)
	    fprintf (f, " (or a derived type)");
	  if (maybe_in_construction)
	    fprintf (f, " (maybe in construction)");
	  fprintf (f, " offset "HOST_WIDE_INT_PRINT_DEC,
		   offset);
	}
      if (speculative_outer_type)
	{
	  if (outer_type || offset)
	    fprintf (f, " ");
	  fprintf (f, "Speculative outer type:");
	  print_generic_expr (f, speculative_outer_type, TDF_SLIM);
	  if (speculative_maybe_derived_type)
	    fprintf (f, " (or a derived type)");
	  fprintf (f, " at offset "HOST_WIDE_INT_PRINT_DEC,
		   speculative_offset);
	}
    }
  if (newline)
    fprintf(f, "\n");
}

/* Print context to stderr.  */

void
ipa_polymorphic_call_context::debug () const
{
  dump (stderr);
}

/* Stream out the context to OB.  */

void
ipa_polymorphic_call_context::stream_out (struct output_block *ob) const
{
  struct bitpack_d bp = bitpack_create (ob->main_stream);

  bp_pack_value (&bp, invalid, 1);
  bp_pack_value (&bp, maybe_in_construction, 1);
  bp_pack_value (&bp, maybe_derived_type, 1);
  bp_pack_value (&bp, speculative_maybe_derived_type, 1);
  bp_pack_value (&bp, dynamic, 1);
  bp_pack_value (&bp, outer_type != NULL, 1);
  bp_pack_value (&bp, offset != 0, 1);
  bp_pack_value (&bp, speculative_outer_type != NULL, 1);
  streamer_write_bitpack (&bp);

  if (outer_type != NULL)
    stream_write_tree (ob, outer_type, true);
  if (offset)
    streamer_write_hwi (ob, offset);
  if (speculative_outer_type != NULL)
    {
      stream_write_tree (ob, speculative_outer_type, true);
      streamer_write_hwi (ob, speculative_offset);
    }
  else
    gcc_assert (!speculative_offset);
}

/* Stream in the context from IB and DATA_IN.  */

void
ipa_polymorphic_call_context::stream_in (struct lto_input_block *ib,
					 struct data_in *data_in)
{
  struct bitpack_d bp = streamer_read_bitpack (ib);

  invalid = bp_unpack_value (&bp, 1);
  maybe_in_construction = bp_unpack_value (&bp, 1);
  maybe_derived_type = bp_unpack_value (&bp, 1);
  speculative_maybe_derived_type = bp_unpack_value (&bp, 1);
  dynamic = bp_unpack_value (&bp, 1);
  bool outer_type_p = bp_unpack_value (&bp, 1);
  bool offset_p = bp_unpack_value (&bp, 1);
  bool speculative_outer_type_p = bp_unpack_value (&bp, 1);

  if (outer_type_p)
    outer_type = stream_read_tree (ib, data_in);
  else
    outer_type = NULL;
  if (offset_p)
    offset = (HOST_WIDE_INT) streamer_read_hwi (ib);
  else
    offset = 0;
  if (speculative_outer_type_p)
    {
      speculative_outer_type = stream_read_tree (ib, data_in);
      speculative_offset = (HOST_WIDE_INT) streamer_read_hwi (ib);
    }
  else
    {
      speculative_outer_type = NULL;
      speculative_offset = 0;
    }
}

/* Proudce polymorphic call context for call method of instance
   that is located within BASE (that is assumed to be a decl) at offset OFF. */

void
ipa_polymorphic_call_context::set_by_decl (tree base, HOST_WIDE_INT off)
{
  gcc_assert (DECL_P (base));
  clear_speculation ();

  if (!contains_polymorphic_type_p (TREE_TYPE (base)))
    {
      clear_outer_type ();
      offset = off;
      return;
    }
  outer_type = TYPE_MAIN_VARIANT (TREE_TYPE (base));
  offset = off;
  /* Make very conservative assumption that all objects
     may be in construction. 
 
     It is up to caller to revisit this via
     get_dynamic_type or decl_maybe_in_construction_p.  */
  maybe_in_construction = true;
  maybe_derived_type = false;
  dynamic = false;
}

/* CST is an invariant (address of decl), try to get meaningful
   polymorphic call context for polymorphic call of method 
   if instance of OTR_TYPE that is located at offset OFF of this invariant.
   Return FALSE if nothing meaningful can be found.  */

bool
ipa_polymorphic_call_context::set_by_invariant (tree cst,
						tree otr_type,
						HOST_WIDE_INT off)
{
  HOST_WIDE_INT offset2, size, max_size;
  tree base;

  invalid = false;
  off = 0;
  clear_outer_type (otr_type);

  if (TREE_CODE (cst) != ADDR_EXPR)
    return false;

  cst = TREE_OPERAND (cst, 0);
  base = get_ref_base_and_extent (cst, &offset2, &size, &max_size);
  if (!DECL_P (base) || max_size == -1 || max_size != size)
    return false;

  /* Only type inconsistent programs can have otr_type that is
     not part of outer type.  */
  if (otr_type && !contains_type_p (TREE_TYPE (base), off, otr_type))
    return false;

  set_by_decl (base, off);
  return true;
}

/* See if OP is SSA name initialized as a copy or by single assignment.
   If so, walk the SSA graph up.  Because simple PHI conditional is considered
   copy, GLOBAL_VISITED may be used to avoid infinite loop walking the SSA
   graph.  */

static tree
walk_ssa_copies (tree op, hash_set<tree> **global_visited = NULL)
{
  hash_set <tree> *visited = NULL;
  STRIP_NOPS (op);
  while (TREE_CODE (op) == SSA_NAME
	 && !SSA_NAME_IS_DEFAULT_DEF (op)
	 /* We might be called via fold_stmt during cfgcleanup where
	    SSA form need not be up-to-date.  */
	 && !name_registered_for_update_p (op) 
	 && (gimple_assign_single_p (SSA_NAME_DEF_STMT (op))
	     || gimple_code (SSA_NAME_DEF_STMT (op)) == GIMPLE_PHI))
    {
      if (global_visited)
	{
	  if (!*global_visited)
	    *global_visited = new hash_set<tree>;
	  if ((*global_visited)->add (op))
	    goto done;
	}	
      else
	{
	  if (!visited)
	    visited = new hash_set<tree>;
	  if (visited->add (op))
	    goto done;
	}
      /* Special case
	 if (ptr == 0)
	   ptr = 0;
	 else
	   ptr = ptr.foo;
	 This pattern is implicitly produced for casts to non-primary
	 bases.  When doing context analysis, we do not really care
	 about the case pointer is NULL, becuase the call will be
	 undefined anyway.  */
      if (gimple_code (SSA_NAME_DEF_STMT (op)) == GIMPLE_PHI)
	{
	  gimple phi = SSA_NAME_DEF_STMT (op);

	  if (gimple_phi_num_args (phi) > 2)
	    goto done;
	  if (gimple_phi_num_args (phi) == 1)
	    op = gimple_phi_arg_def (phi, 0);
	  else if (integer_zerop (gimple_phi_arg_def (phi, 0)))
	    op = gimple_phi_arg_def (phi, 1);
	  else if (integer_zerop (gimple_phi_arg_def (phi, 1)))
	    op = gimple_phi_arg_def (phi, 0);
	  else
	    goto done;
	}
      else
	{
	  if (gimple_assign_load_p (SSA_NAME_DEF_STMT (op)))
	    goto done;
	  op = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (op));
	}
      STRIP_NOPS (op);
    }
done:
  if (visited)
    delete (visited);
  return op;
}

/* Create polymorphic call context from IP invariant CST.
   This is typically &global_var.
   OTR_TYPE specify type of polymorphic call or NULL if unknown, OFF
   is offset of call.  */

ipa_polymorphic_call_context::ipa_polymorphic_call_context (tree cst,
							    tree otr_type,
							    HOST_WIDE_INT off)
{
  clear_speculation ();
  set_by_invariant (cst, otr_type, off);
}

/* Build context for pointer REF contained in FNDECL at statement STMT.
   if INSTANCE is non-NULL, return pointer to the object described by
   the context or DECL where context is contained in.  */

ipa_polymorphic_call_context::ipa_polymorphic_call_context (tree fndecl,
							    tree ref,
							    gimple stmt,
							    tree *instance)
{
  tree otr_type = NULL;
  tree base_pointer;
  hash_set <tree> *visited = NULL;

  if (TREE_CODE (ref) == OBJ_TYPE_REF)
    {
      otr_type = obj_type_ref_class (ref);
      base_pointer = OBJ_TYPE_REF_OBJECT (ref);
    }
  else
    base_pointer = ref;

  /* Set up basic info in case we find nothing interesting in the analysis.  */
  clear_speculation ();
  clear_outer_type (otr_type);
  invalid = false;

  /* Walk SSA for outer object.  */
  while (true)
    {
      base_pointer = walk_ssa_copies (base_pointer, &visited);
      if (TREE_CODE (base_pointer) == ADDR_EXPR)
	{
	  HOST_WIDE_INT size, max_size;
	  HOST_WIDE_INT offset2;
	  tree base = get_ref_base_and_extent (TREE_OPERAND (base_pointer, 0),
					       &offset2, &size, &max_size);

	  if (max_size != -1 && max_size == size)
	    combine_speculation_with (TYPE_MAIN_VARIANT (TREE_TYPE (base)),
				      offset + offset2,
				      true,
				      NULL /* Do not change outer type.  */);

	  /* If this is a varying address, punt.  */
	  if ((TREE_CODE (base) == MEM_REF || DECL_P (base))
	      && max_size != -1
	      && max_size == size)
	    {
	      /* We found dereference of a pointer.  Type of the pointer
		 and MEM_REF is meaningless, but we can look futher.  */
	      if (TREE_CODE (base) == MEM_REF)
		{
		  base_pointer = TREE_OPERAND (base, 0);
		  offset
		    += offset2 + mem_ref_offset (base).to_short_addr () * BITS_PER_UNIT;
		  outer_type = NULL;
		}
	      /* We found base object.  In this case the outer_type
		 is known.  */
	      else if (DECL_P (base))
		{
		  if (visited)
		    delete (visited);
		  /* Only type inconsistent programs can have otr_type that is
		     not part of outer type.  */
		  if (otr_type
		      && !contains_type_p (TREE_TYPE (base),
					   offset + offset2, otr_type))
		    {
		      invalid = true;
		      if (instance)
			*instance = base_pointer;
		      return;
		    }
		  set_by_decl (base, offset + offset2);
		  if (outer_type && maybe_in_construction && stmt)
		    maybe_in_construction
		     = decl_maybe_in_construction_p (base,
						     outer_type,
						     stmt,
						     fndecl);
		  if (instance)
		    *instance = base;
		  return;
		}
	      else
		break;
	    }
	  else
	    break;
	}
      else if (TREE_CODE (base_pointer) == POINTER_PLUS_EXPR
	       && tree_fits_uhwi_p (TREE_OPERAND (base_pointer, 1)))
	{
	  offset += tree_to_shwi (TREE_OPERAND (base_pointer, 1))
		    * BITS_PER_UNIT;
	  base_pointer = TREE_OPERAND (base_pointer, 0);
	}
      else
	break;
    }

  if (visited)
    delete (visited);

  /* Try to determine type of the outer object.  */
  if (TREE_CODE (base_pointer) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (base_pointer)
      && TREE_CODE (SSA_NAME_VAR (base_pointer)) == PARM_DECL)
    {
      /* See if parameter is THIS pointer of a method.  */
      if (TREE_CODE (TREE_TYPE (fndecl)) == METHOD_TYPE
	  && SSA_NAME_VAR (base_pointer) == DECL_ARGUMENTS (fndecl))
	{
	  outer_type
	     = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (base_pointer)));
	  gcc_assert (TREE_CODE (outer_type) == RECORD_TYPE
		      || TREE_CODE (outer_type) == UNION_TYPE);

	  /* Dynamic casting has possibly upcasted the type
	     in the hiearchy.  In this case outer type is less
	     informative than inner type and we should forget
	     about it.  */
	  if ((otr_type
	       && !contains_type_p (outer_type, offset,
				    otr_type))
	      || !contains_polymorphic_type_p (outer_type))
	    {
	      outer_type = NULL;
	      if (instance)
		*instance = base_pointer;
	      return;
	    }

	  dynamic = true;

	  /* If the function is constructor or destructor, then
	     the type is possibly in construction, but we know
	     it is not derived type.  */
	  if (DECL_CXX_CONSTRUCTOR_P (fndecl)
	      || DECL_CXX_DESTRUCTOR_P (fndecl))
	    {
	      maybe_in_construction = true;
	      maybe_derived_type = false;
	    }
	  else
	    {
	      maybe_derived_type = true;
	      maybe_in_construction = false;
	    }
	  if (instance)
	    *instance = base_pointer;
	  return;
	}
      /* Non-PODs passed by value are really passed by invisible
	 reference.  In this case we also know the type of the
	 object.  */
      if (DECL_BY_REFERENCE (SSA_NAME_VAR (base_pointer)))
	{
	  outer_type
	     = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (base_pointer)));
	  /* Only type inconsistent programs can have otr_type that is
	     not part of outer type.  */
	  if (otr_type && !contains_type_p (outer_type, offset,
					    otr_type))
	    { 
	      invalid = true;
	      if (instance)
		*instance = base_pointer;
	      return;
	    }
	  /* Non-polymorphic types have no interest for us.  */
	  else if (!otr_type && !contains_polymorphic_type_p (outer_type))
	    {
	      outer_type = NULL;
	      if (instance)
		*instance = base_pointer;
	      return;
	    }
	  maybe_derived_type = false;
	  maybe_in_construction = false;
	  if (instance)
	    *instance = base_pointer;
	  return;
	}
    }

  tree base_type = TREE_TYPE (base_pointer);

  if (TREE_CODE (base_pointer) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (base_pointer)
      && !(TREE_CODE (SSA_NAME_VAR (base_pointer)) == PARM_DECL
	   || TREE_CODE (SSA_NAME_VAR (base_pointer)) == RESULT_DECL))
    {
      invalid = true;
      if (instance)
	*instance = base_pointer;
      return;
    }
  if (TREE_CODE (base_pointer) == SSA_NAME
      && SSA_NAME_DEF_STMT (base_pointer)
      && gimple_assign_single_p (SSA_NAME_DEF_STMT (base_pointer)))
    base_type = TREE_TYPE (gimple_assign_rhs1
			    (SSA_NAME_DEF_STMT (base_pointer)));
 
  if (base_type && POINTER_TYPE_P (base_type))
    combine_speculation_with (TYPE_MAIN_VARIANT (TREE_TYPE (base_type)),
			      offset,
			      true, NULL /* Do not change type here */);
  /* TODO: There are multiple ways to derive a type.  For instance
     if BASE_POINTER is passed to an constructor call prior our refernece.
     We do not make this type of flow sensitive analysis yet.  */
  if (instance)
    *instance = base_pointer;
  return;
}

/* Structure to be passed in between detect_type_change and
   check_stmt_for_type_change.  */

struct type_change_info
{
  /* Offset into the object where there is the virtual method pointer we are
     looking for.  */
  HOST_WIDE_INT offset;
  /* The declaration or SSA_NAME pointer of the base that we are checking for
     type change.  */
  tree instance;
  /* The reference to virtual table pointer used.  */
  tree vtbl_ptr_ref;
  tree otr_type;
  /* If we actually can tell the type that the object has changed to, it is
     stored in this field.  Otherwise it remains NULL_TREE.  */
  tree known_current_type;
  HOST_WIDE_INT known_current_offset;

  /* Set to true if dynamic type change has been detected.  */
  bool type_maybe_changed;
  /* Set to true if multiple types have been encountered.  known_current_type
     must be disregarded in that case.  */
  bool multiple_types_encountered;
  /* Set to true if we possibly missed some dynamic type changes and we should
     consider the set to be speculative.  */
  bool speculative;
  bool seen_unanalyzed_store;
};

/* Return true if STMT is not call and can modify a virtual method table pointer.
   We take advantage of fact that vtable stores must appear within constructor
   and destructor functions.  */

static bool
noncall_stmt_may_be_vtbl_ptr_store (gimple stmt)
{
  if (is_gimple_assign (stmt))
    {
      tree lhs = gimple_assign_lhs (stmt);

      if (gimple_clobber_p (stmt))
	return false;
      if (!AGGREGATE_TYPE_P (TREE_TYPE (lhs)))
	{
	  if (flag_strict_aliasing
	      && !POINTER_TYPE_P (TREE_TYPE (lhs)))
	    return false;

	  if (TREE_CODE (lhs) == COMPONENT_REF
	      && !DECL_VIRTUAL_P (TREE_OPERAND (lhs, 1)))
	    return false;
	  /* In the future we might want to use get_base_ref_and_offset to find
	     if there is a field corresponding to the offset and if so, proceed
	     almost like if it was a component ref.  */
	}
    }

  /* Code unification may mess with inline stacks.  */
  if (cfun->after_inlining)
    return true;

  /* Walk the inline stack and watch out for ctors/dtors.
     TODO: Maybe we can require the store to appear in toplevel
     block of CTOR/DTOR.  */
  for (tree block = gimple_block (stmt); block && TREE_CODE (block) == BLOCK;
       block = BLOCK_SUPERCONTEXT (block))
    if (BLOCK_ABSTRACT_ORIGIN (block)
	&& TREE_CODE (block_ultimate_origin (block)) == FUNCTION_DECL)
      return inlined_polymorphic_ctor_dtor_block_p (block, false);
  return (TREE_CODE (TREE_TYPE (current_function_decl)) == METHOD_TYPE
	  && (DECL_CXX_CONSTRUCTOR_P (current_function_decl)
	      || DECL_CXX_DESTRUCTOR_P (current_function_decl)));
}

/* If STMT can be proved to be an assignment to the virtual method table
   pointer of ANALYZED_OBJ and the type associated with the new table
   identified, return the type.  Otherwise return NULL_TREE if type changes
   in unknown way or ERROR_MARK_NODE if type is unchanged.  */

static tree
extr_type_from_vtbl_ptr_store (gimple stmt, struct type_change_info *tci,
			       HOST_WIDE_INT *type_offset)
{
  HOST_WIDE_INT offset, size, max_size;
  tree lhs, rhs, base;

  if (!gimple_assign_single_p (stmt))
    return NULL_TREE;

  lhs = gimple_assign_lhs (stmt);
  rhs = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (lhs) != COMPONENT_REF
      || !DECL_VIRTUAL_P (TREE_OPERAND (lhs, 1)))
     {
	if (dump_file)
	  fprintf (dump_file, "  LHS is not virtual table.\n");
	return NULL_TREE;
     }

  if (tci->vtbl_ptr_ref && operand_equal_p (lhs, tci->vtbl_ptr_ref, 0))
    ;
  else
    {
      base = get_ref_base_and_extent (lhs, &offset, &size, &max_size);
      if (DECL_P (tci->instance))
	{
	  if (base != tci->instance)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "    base:");
		  print_generic_expr (dump_file, base, TDF_SLIM);
		  fprintf (dump_file, " does not match instance:");
		  print_generic_expr (dump_file, tci->instance, TDF_SLIM);
		  fprintf (dump_file, "\n");
		}
	      return NULL_TREE;
	    }
	}
      else if (TREE_CODE (base) == MEM_REF)
	{
	  if (!operand_equal_p (tci->instance, TREE_OPERAND (base, 0), 0))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "    base mem ref:");
		  print_generic_expr (dump_file, base, TDF_SLIM);
		  fprintf (dump_file, " does not match instance:");
		  print_generic_expr (dump_file, tci->instance, TDF_SLIM);
		  fprintf (dump_file, "\n");
		}
	      return NULL_TREE;
	    }
	  if (!integer_zerop (TREE_OPERAND (base, 1)))
	    {
	      if (!tree_fits_shwi_p (TREE_OPERAND (base, 1)))
		{
		  if (dump_file)
		    {
		      fprintf (dump_file, "    base mem ref:");
		      print_generic_expr (dump_file, base, TDF_SLIM);
		      fprintf (dump_file, " has non-representable offset:");
		      print_generic_expr (dump_file, tci->instance, TDF_SLIM);
		      fprintf (dump_file, "\n");
		    }
		  return NULL_TREE;
		}
	      else
	        offset += tree_to_shwi (TREE_OPERAND (base, 1)) * BITS_PER_UNIT;
	    }
	}
      else if (!operand_equal_p (tci->instance, base, 0)
	       || tci->offset)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "    base:");
	      print_generic_expr (dump_file, base, TDF_SLIM);
	      fprintf (dump_file, " does not match instance:");
	      print_generic_expr (dump_file, tci->instance, TDF_SLIM);
	      fprintf (dump_file, " with offset %i\n", (int)tci->offset);
	    }
	  return tci->offset > POINTER_SIZE ? error_mark_node : NULL_TREE;
	}
      if (offset != tci->offset
	  || size != POINTER_SIZE
	  || max_size != POINTER_SIZE)
	{
	  if (dump_file)
	    fprintf (dump_file, "    wrong offset %i!=%i or size %i\n",
		     (int)offset, (int)tci->offset, (int)size);
	  return offset + POINTER_SIZE <= tci->offset
	         || (max_size != -1
		     && tci->offset + POINTER_SIZE > offset + max_size)
		 ? error_mark_node : NULL;
	}
    }

  tree vtable;
  unsigned HOST_WIDE_INT offset2;

  if (!vtable_pointer_value_to_vtable (rhs, &vtable, &offset2))
    {
      if (dump_file)
	fprintf (dump_file, "    Failed to lookup binfo\n");
      return NULL;
    }

  tree binfo = subbinfo_with_vtable_at_offset (TYPE_BINFO (DECL_CONTEXT (vtable)),
					       offset2, vtable);
  if (!binfo)
    {
      if (dump_file)
	fprintf (dump_file, "    Construction vtable used\n");
      /* FIXME: We should suport construction contexts.  */
      return NULL;
    }
 
  *type_offset = tree_to_shwi (BINFO_OFFSET (binfo)) * BITS_PER_UNIT;
  return DECL_CONTEXT (vtable);
}

/* Record dynamic type change of TCI to TYPE.  */

static void
record_known_type (struct type_change_info *tci, tree type, HOST_WIDE_INT offset)
{
  if (dump_file)
    {
      if (type)
	{
          fprintf (dump_file, "  Recording type: ");
	  print_generic_expr (dump_file, type, TDF_SLIM);
          fprintf (dump_file, " at offset %i\n", (int)offset);
	}
     else
       fprintf (dump_file, "  Recording unknown type\n");
    }

  /* If we found a constructor of type that is not polymorphic or
     that may contain the type in question as a field (not as base),
     restrict to the inner class first to make type matching bellow
     happier.  */
  if (type
      && (offset
          || (TREE_CODE (type) != RECORD_TYPE
	      || !TYPE_BINFO (type)
	      || !polymorphic_type_binfo_p (TYPE_BINFO (type)))))
    {
      ipa_polymorphic_call_context context;

      context.offset = offset;
      context.outer_type = type;
      context.maybe_in_construction = false;
      context.maybe_derived_type = false;
      context.dynamic = true;
      /* If we failed to find the inner type, we know that the call
	 would be undefined for type produced here.  */
      if (!context.restrict_to_inner_class (tci->otr_type))
	{
	  if (dump_file)
	    fprintf (dump_file, "  Ignoring; does not contain otr_type\n");
	  return;
	}
      /* Watch for case we reached an POD type and anticipate placement
	 new.  */
      if (!context.maybe_derived_type)
	{
          type = context.outer_type;
          offset = context.offset;
	}
    }
  if (tci->type_maybe_changed
      && (!types_same_for_odr (type, tci->known_current_type)
	  || offset != tci->known_current_offset))
    tci->multiple_types_encountered = true;
  tci->known_current_type = TYPE_MAIN_VARIANT (type);
  tci->known_current_offset = offset;
  tci->type_maybe_changed = true;
}

/* Callback of walk_aliased_vdefs and a helper function for
   detect_type_change to check whether a particular statement may modify
   the virtual table pointer, and if possible also determine the new type of
   the (sub-)object.  It stores its result into DATA, which points to a
   type_change_info structure.  */

static bool
check_stmt_for_type_change (ao_ref *ao ATTRIBUTE_UNUSED, tree vdef, void *data)
{
  gimple stmt = SSA_NAME_DEF_STMT (vdef);
  struct type_change_info *tci = (struct type_change_info *) data;
  tree fn;

  /* If we already gave up, just terminate the rest of walk.  */
  if (tci->multiple_types_encountered)
    return true;

  if (is_gimple_call (stmt))
    {
      if (gimple_call_flags (stmt) & (ECF_CONST | ECF_PURE))
	return false;

      /* Check for a constructor call.  */
      if ((fn = gimple_call_fndecl (stmt)) != NULL_TREE
	  && DECL_CXX_CONSTRUCTOR_P (fn)
	  && TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE
	  && gimple_call_num_args (stmt))
      {
	tree op = walk_ssa_copies (gimple_call_arg (stmt, 0));
	tree type = method_class_type (TREE_TYPE (fn));
	HOST_WIDE_INT offset = 0, size, max_size;

	if (dump_file)
	  {
	    fprintf (dump_file, "  Checking constructor call: ");
	    print_gimple_stmt (dump_file, stmt, 0, 0);
	  }

	/* See if THIS parameter seems like instance pointer.  */
	if (TREE_CODE (op) == ADDR_EXPR)
	  {
	    op = get_ref_base_and_extent (TREE_OPERAND (op, 0),
					  &offset, &size, &max_size);
	    if (size != max_size || max_size == -1)
	      {
                tci->speculative = true;
	        return false;
	      }
	    if (op && TREE_CODE (op) == MEM_REF)
	      {
		if (!tree_fits_shwi_p (TREE_OPERAND (op, 1)))
		  {
                    tci->speculative = true;
		    return false;
		  }
		offset += tree_to_shwi (TREE_OPERAND (op, 1))
			  * BITS_PER_UNIT;
		op = TREE_OPERAND (op, 0);
	      }
	    else if (DECL_P (op))
	      ;
	    else
	      {
                tci->speculative = true;
	        return false;
	      }
	    op = walk_ssa_copies (op);
	  }
	if (operand_equal_p (op, tci->instance, 0)
	    && TYPE_SIZE (type)
	    && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
	    && tree_fits_shwi_p (TYPE_SIZE (type))
	    && tree_to_shwi (TYPE_SIZE (type)) + offset > tci->offset
	    /* Some inlined constructors may look as follows:
		  _3 = operator new (16);
		  MEM[(struct  &)_3] ={v} {CLOBBER};
		  MEM[(struct CompositeClass *)_3]._vptr.CompositeClass
		    = &MEM[(void *)&_ZTV14CompositeClass + 16B];
		  _7 = &MEM[(struct CompositeClass *)_3].object;
		  EmptyClass::EmptyClass (_7);

	       When determining dynamic type of _3 and because we stop at first
	       dynamic type found, we would stop on EmptyClass::EmptyClass (_7).
	       In this case the emptyclass is not even polymorphic and we miss
	       it is contained in an outer type that is polymorphic.  */

	    && (tci->offset == offset || contains_polymorphic_type_p (type)))
	  {
	    record_known_type (tci, type, tci->offset - offset);
	    return true;
	  }
      }
     /* Calls may possibly change dynamic type by placement new. Assume
        it will not happen, but make result speculative only.  */
     if (dump_file)
	{
          fprintf (dump_file, "  Function call may change dynamic type:");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	}
     tci->speculative = true;
     return false;
   }
  /* Check for inlined virtual table store.  */
  else if (noncall_stmt_may_be_vtbl_ptr_store (stmt))
    {
      tree type;
      HOST_WIDE_INT offset = 0;
      if (dump_file)
	{
	  fprintf (dump_file, "  Checking vtbl store: ");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	}

      type = extr_type_from_vtbl_ptr_store (stmt, tci, &offset);
      if (type == error_mark_node)
	return false;
      gcc_assert (!type || TYPE_MAIN_VARIANT (type) == type);
      if (!type)
	{
	  if (dump_file)
	    fprintf (dump_file, "  Unanalyzed store may change type.\n");
	  tci->seen_unanalyzed_store = true;
	  tci->speculative = true;
	}
      else
        record_known_type (tci, type, offset);
      return true;
    }
  else
    return false;
}

/* THIS is polymorphic call context obtained from get_polymorphic_context.
   OTR_OBJECT is pointer to the instance returned by OBJ_TYPE_REF_OBJECT.
   INSTANCE is pointer to the outer instance as returned by
   get_polymorphic_context.  To avoid creation of temporary expressions,
   INSTANCE may also be an declaration of get_polymorphic_context found the
   value to be in static storage.

   If the type of instance is not fully determined
   (either OUTER_TYPE is unknown or MAYBE_IN_CONSTRUCTION/INCLUDE_DERIVED_TYPES
   is set), try to walk memory writes and find the actual construction of the
   instance.

   Return true if memory is unchanged from function entry.

   We do not include this analysis in the context analysis itself, because
   it needs memory SSA to be fully built and the walk may be expensive.
   So it is not suitable for use withing fold_stmt and similar uses.  */

bool
ipa_polymorphic_call_context::get_dynamic_type (tree instance,
						tree otr_object,
						tree otr_type,
						gimple call)
{
  struct type_change_info tci;
  ao_ref ao;
  bool function_entry_reached = false;
  tree instance_ref = NULL;
  gimple stmt = call;
  /* Remember OFFSET before it is modified by restrict_to_inner_class.
     This is because we do not update INSTANCE when walking inwards.  */
  HOST_WIDE_INT instance_offset = offset;
  tree instance_outer_type = outer_type;

  if (otr_type)
    otr_type = TYPE_MAIN_VARIANT (otr_type);

  /* Walk into inner type. This may clear maybe_derived_type and save us
     from useless work.  It also makes later comparsions with static type
     easier.  */
  if (outer_type && otr_type)
    {
      if (!restrict_to_inner_class (otr_type))
        return false;
    }

  if (!maybe_in_construction && !maybe_derived_type)
    return false;

  /* We need to obtain refernce to virtual table pointer.  It is better
     to look it up in the code rather than build our own.  This require bit
     of pattern matching, but we end up verifying that what we found is
     correct. 

     What we pattern match is:

       tmp = instance->_vptr.A;   // vtbl ptr load
       tmp2 = tmp[otr_token];	  // vtable lookup
       OBJ_TYPE_REF(tmp2;instance->0) (instance);
 
     We want to start alias oracle walk from vtbl pointer load,
     but we may not be able to identify it, for example, when PRE moved the
     load around.  */

  if (gimple_code (call) == GIMPLE_CALL)
    {
      tree ref = gimple_call_fn (call);
      HOST_WIDE_INT offset2, size, max_size;

      if (TREE_CODE (ref) == OBJ_TYPE_REF)
	{
	  ref = OBJ_TYPE_REF_EXPR (ref);
	  ref = walk_ssa_copies (ref);

	  /* Check if definition looks like vtable lookup.  */
	  if (TREE_CODE (ref) == SSA_NAME
	      && !SSA_NAME_IS_DEFAULT_DEF (ref)
	      && gimple_assign_load_p (SSA_NAME_DEF_STMT (ref))
	      && TREE_CODE (gimple_assign_rhs1
			     (SSA_NAME_DEF_STMT (ref))) == MEM_REF)
	    {
	      ref = get_base_address
		     (TREE_OPERAND (gimple_assign_rhs1
				     (SSA_NAME_DEF_STMT (ref)), 0));
	      ref = walk_ssa_copies (ref);
	      /* Find base address of the lookup and see if it looks like
		 vptr load.  */
	      if (TREE_CODE (ref) == SSA_NAME
		  && !SSA_NAME_IS_DEFAULT_DEF (ref)
		  && gimple_assign_load_p (SSA_NAME_DEF_STMT (ref)))
		{
		  tree ref_exp = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (ref));
		  tree base_ref = get_ref_base_and_extent
				   (ref_exp, &offset2, &size, &max_size);

		  /* Finally verify that what we found looks like read from OTR_OBJECT
		     or from INSTANCE with offset OFFSET.  */
		  if (base_ref
		      && ((TREE_CODE (base_ref) == MEM_REF
		           && ((offset2 == instance_offset
		                && TREE_OPERAND (base_ref, 0) == instance)
			       || (!offset2 && TREE_OPERAND (base_ref, 0) == otr_object)))
			  || (DECL_P (instance) && base_ref == instance
			      && offset2 == instance_offset)))
		    {
		      stmt = SSA_NAME_DEF_STMT (ref);
		      instance_ref = ref_exp;
		    }
		}
	    }
	}
    }
 
  /* If we failed to look up the reference in code, build our own.  */
  if (!instance_ref)
    {
      /* If the statement in question does not use memory, we can't tell
	 anything.  */
      if (!gimple_vuse (stmt))
	return false;
      ao_ref_init_from_ptr_and_size (&ao, otr_object, NULL);
    }
  else
  /* Otherwise use the real reference.  */
    ao_ref_init (&ao, instance_ref);

  /* We look for vtbl pointer read.  */
  ao.size = POINTER_SIZE;
  ao.max_size = ao.size;
  if (otr_type)
    ao.ref_alias_set
      = get_deref_alias_set (TREE_TYPE (BINFO_VTABLE (TYPE_BINFO (otr_type))));

  if (dump_file)
    {
      fprintf (dump_file, "Determining dynamic type for call: ");
      print_gimple_stmt (dump_file, call, 0, 0);
      fprintf (dump_file, "  Starting walk at: ");
      print_gimple_stmt (dump_file, stmt, 0, 0);
      fprintf (dump_file, "  instance pointer: ");
      print_generic_expr (dump_file, otr_object, TDF_SLIM);
      fprintf (dump_file, "  Outer instance pointer: ");
      print_generic_expr (dump_file, instance, TDF_SLIM);
      fprintf (dump_file, " offset: %i (bits)", (int)instance_offset);
      fprintf (dump_file, " vtbl reference: ");
      print_generic_expr (dump_file, instance_ref, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  tci.offset = instance_offset;
  tci.instance = instance;
  tci.vtbl_ptr_ref = instance_ref;
  gcc_assert (TREE_CODE (instance) != MEM_REF);
  tci.known_current_type = NULL_TREE;
  tci.known_current_offset = 0;
  tci.otr_type = otr_type;
  tci.type_maybe_changed = false;
  tci.multiple_types_encountered = false;
  tci.speculative = false;
  tci.seen_unanalyzed_store = false;

  walk_aliased_vdefs (&ao, gimple_vuse (stmt), check_stmt_for_type_change,
		      &tci, NULL, &function_entry_reached);

  /* If we did not find any type changing statements, we may still drop
     maybe_in_construction flag if the context already have outer type. 

     Here we make special assumptions about both constructors and
     destructors which are all the functions that are allowed to alter the
     VMT pointers.  It assumes that destructors begin with assignment into
     all VMT pointers and that constructors essentially look in the
     following way:

     1) The very first thing they do is that they call constructors of
     ancestor sub-objects that have them.

     2) Then VMT pointers of this and all its ancestors is set to new
     values corresponding to the type corresponding to the constructor.

     3) Only afterwards, other stuff such as constructor of member
     sub-objects and the code written by the user is run.  Only this may
     include calling virtual functions, directly or indirectly.

     4) placement new can not be used to change type of non-POD statically
     allocated variables.

     There is no way to call a constructor of an ancestor sub-object in any
     other way.

     This means that we do not have to care whether constructors get the
     correct type information because they will always change it (in fact,
     if we define the type to be given by the VMT pointer, it is undefined).

     The most important fact to derive from the above is that if, for some
     statement in the section 3, we try to detect whether the dynamic type
     has changed, we can safely ignore all calls as we examine the function
     body backwards until we reach statements in section 2 because these
     calls cannot be ancestor constructors or destructors (if the input is
     not bogus) and so do not change the dynamic type (this holds true only
     for automatically allocated objects but at the moment we devirtualize
     only these).  We then must detect that statements in section 2 change
     the dynamic type and can try to derive the new type.  That is enough
     and we can stop, we will never see the calls into constructors of
     sub-objects in this code. 

     Therefore if the static outer type was found (outer_type)
     we can safely ignore tci.speculative that is set on calls and give up
     only if there was dyanmic type store that may affect given variable
     (seen_unanalyzed_store)  */

  if (!tci.type_maybe_changed
      || (outer_type
	  && !dynamic
	  && !tci.seen_unanalyzed_store
	  && !tci.multiple_types_encountered
	  && ((offset == tci.offset
	       && types_same_for_odr (tci.known_current_type,
				      outer_type))
	       || (instance_offset == offset
		   && types_same_for_odr (tci.known_current_type,
					  instance_outer_type)))))
    {
      if (!outer_type || tci.seen_unanalyzed_store)
	return false;
      if (maybe_in_construction)
        maybe_in_construction = false;
      if (dump_file)
	fprintf (dump_file, "  No dynamic type change found.\n");
      return true;
    }

  if (tci.known_current_type
      && !function_entry_reached
      && !tci.multiple_types_encountered)
    {
      if (!tci.speculative)
	{
	  outer_type = TYPE_MAIN_VARIANT (tci.known_current_type);
	  offset = tci.known_current_offset;
	  dynamic = true;
	  maybe_in_construction = false;
	  maybe_derived_type = false;
	  if (dump_file)
	    fprintf (dump_file, "  Determined dynamic type.\n");
	}
      else if (!speculative_outer_type
	       || speculative_maybe_derived_type)
	{
	  speculative_outer_type = TYPE_MAIN_VARIANT (tci.known_current_type);
	  speculative_offset = tci.known_current_offset;
	  speculative_maybe_derived_type = false;
	  if (dump_file)
	    fprintf (dump_file, "  Determined speculative dynamic type.\n");
	}
    }
  else if (dump_file)
    {
      fprintf (dump_file, "  Found multiple types%s%s\n",
	       function_entry_reached ? " (function entry reached)" : "",
	       function_entry_reached ? " (multiple types encountered)" : "");
    }

  return false;
}

/* See if speculation given by SPEC_OUTER_TYPE, SPEC_OFFSET and SPEC_MAYBE_DERIVED_TYPE
   seems consistent (and useful) with what we already have in the non-speculative context.  */

bool
ipa_polymorphic_call_context::speculation_consistent_p (tree spec_outer_type,
							HOST_WIDE_INT spec_offset,
							bool spec_maybe_derived_type,
							tree otr_type) const
{
  if (!flag_devirtualize_speculatively)
    return false;

  /* Non-polymorphic types are useless for deriving likely polymorphic
     call targets.  */
  if (!spec_outer_type || !contains_polymorphic_type_p (spec_outer_type))
    return false;

  /* If we know nothing, speculation is always good.  */
  if (!outer_type)
    return true;

  /* Speculation is only useful to avoid derived types.
     This is not 100% true for placement new, where the outer context may
     turn out to be useless, but ignore these for now.  */
  if (!maybe_derived_type)
    return false;

  /* If types agrees, speculation is consistent, but it makes sense only
     when it says something new.  */
  if (types_must_be_same_for_odr (spec_outer_type, outer_type))
    return maybe_derived_type && !spec_maybe_derived_type;

  /* If speculation does not contain the type in question, ignore it.  */
  if (otr_type
      && !contains_type_p (spec_outer_type, spec_offset, otr_type, false, true))
    return false;

  /* If outer type already contains speculation as a filed,
     it is useless.  We already know from OUTER_TYPE 
     SPEC_TYPE and that it is not in the construction.  */
  if (contains_type_p (outer_type, offset - spec_offset,
		       spec_outer_type, false, false))
    return false;

  /* If speculative outer type is not more specified than outer
     type, just give up. 
     We can only decide this safely if we can compare types with OUTER_TYPE.
   */
  if ((!in_lto_p || odr_type_p (outer_type))
      && !contains_type_p (spec_outer_type,
			   spec_offset - offset,
			   outer_type, false))
    return false;
  return true;
}

/* Improve THIS with speculation described by NEW_OUTER_TYPE, NEW_OFFSET,
   NEW_MAYBE_DERIVED_TYPE 
   If OTR_TYPE is set, assume the context is used with OTR_TYPE.  */

bool
ipa_polymorphic_call_context::combine_speculation_with
   (tree new_outer_type, HOST_WIDE_INT new_offset, bool new_maybe_derived_type,
    tree otr_type)
{
  if (!new_outer_type)
    return false;

  /* restrict_to_inner_class may eliminate wrong speculation making our job
     easeier.  */
  if (otr_type)
    restrict_to_inner_class (otr_type);

  if (!speculation_consistent_p (new_outer_type, new_offset,
				 new_maybe_derived_type, otr_type))
    return false;

  /* New speculation is a win in case we have no speculation or new
     speculation does not consider derivations.  */
  if (!speculative_outer_type
      || (speculative_maybe_derived_type
	  && !new_maybe_derived_type))
    {
      speculative_outer_type = new_outer_type;
      speculative_offset = new_offset;
      speculative_maybe_derived_type = new_maybe_derived_type;
      return true;
    }
  else if (types_must_be_same_for_odr (speculative_outer_type,
				       new_outer_type))
    {
      if (speculative_offset != new_offset)
	{
	  /* OK we have two contexts that seems valid but they disagree,
	     just give up.

	     This is not a lattice operation, so we may want to drop it later.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Speculative outer types match, "
		     "offset mismatch -> invalid speculation\n");
	  clear_speculation ();
	  return true;
	}
      else
	{
	  if (speculative_maybe_derived_type && !new_maybe_derived_type)
	    {
	      speculative_maybe_derived_type = false;
	      return true;
	    }
	  else
	    return false;
	}
    }
  /* Choose type that contains the other.  This one either contains the outer
     as a field (thus giving exactly one target) or is deeper in the type
     hiearchy.  */
  else if (speculative_outer_type
	   && speculative_maybe_derived_type
	   && (new_offset > speculative_offset
	       || (new_offset == speculative_offset
		   && contains_type_p (new_outer_type,
				       0, speculative_outer_type, false))))
    {
      tree old_outer_type = speculative_outer_type;
      HOST_WIDE_INT old_offset = speculative_offset;
      bool old_maybe_derived_type = speculative_maybe_derived_type;

      speculative_outer_type = new_outer_type;
      speculative_offset = new_offset;
      speculative_maybe_derived_type = new_maybe_derived_type;

      if (otr_type)
	restrict_to_inner_class (otr_type);

      /* If the speculation turned out to make no sense, revert to sensible
	 one.  */
      if (!speculative_outer_type)
	{
	  speculative_outer_type = old_outer_type;
	  speculative_offset = old_offset;
	  speculative_maybe_derived_type = old_maybe_derived_type;
	  return false;
	}
      return (old_offset != speculative_offset
	      || old_maybe_derived_type != speculative_maybe_derived_type
	      || types_must_be_same_for_odr (speculative_outer_type,
					     new_outer_type));
    }
  return false;
}

/* Make speculation less specific so
   NEW_OUTER_TYPE, NEW_OFFSET, NEW_MAYBE_DERIVED_TYPE is also included.
   If OTR_TYPE is set, assume the context is used with OTR_TYPE.  */

bool
ipa_polymorphic_call_context::meet_speculation_with
   (tree new_outer_type, HOST_WIDE_INT new_offset, bool new_maybe_derived_type,
    tree otr_type)
{
  if (!new_outer_type && speculative_outer_type)
    {
      clear_speculation ();
      return true;
    }

  /* restrict_to_inner_class may eliminate wrong speculation making our job
     easeier.  */
  if (otr_type)
    restrict_to_inner_class (otr_type);

  if (!speculative_outer_type
      || !speculation_consistent_p (speculative_outer_type,
				    speculative_offset,
				    speculative_maybe_derived_type,
				    otr_type))
    return false;

  if (!speculation_consistent_p (new_outer_type, new_offset,
				 new_maybe_derived_type, otr_type))
    {
      clear_speculation ();
      return true;
    }

  else if (types_must_be_same_for_odr (speculative_outer_type,
				       new_outer_type))
    {
      if (speculative_offset != new_offset)
	{
	  clear_speculation ();
	  return true;
	}
      else
	{
	  if (!speculative_maybe_derived_type && new_maybe_derived_type)
	    {
	      speculative_maybe_derived_type = true;
	      return true;
	    }
	  else
	    return false;
	}
    }
  /* See if one type contains the other as a field (not base).  */
  else if (contains_type_p (new_outer_type, new_offset - speculative_offset,
			    speculative_outer_type, false, false))
    return false;
  else if (contains_type_p (speculative_outer_type,
			    speculative_offset - new_offset,
			    new_outer_type, false, false))
    {
      speculative_outer_type = new_outer_type;
      speculative_offset = new_offset;
      speculative_maybe_derived_type = new_maybe_derived_type;
      return true;
    }
  /* See if OUTER_TYPE is base of CTX.OUTER_TYPE.  */
  else if (contains_type_p (new_outer_type,
			    new_offset - speculative_offset,
			    speculative_outer_type, false, true))
    {
      if (!speculative_maybe_derived_type)
	{
	  speculative_maybe_derived_type = true;
	  return true;
	}
      return false;
    }
  /* See if CTX.OUTER_TYPE is base of OUTER_TYPE.  */
  else if (contains_type_p (speculative_outer_type,
			    speculative_offset - new_offset, new_outer_type, false, true))
    {
      speculative_outer_type = new_outer_type;
      speculative_offset = new_offset;
      speculative_maybe_derived_type = true;
      return true;
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "Giving up on speculative meet\n");
      clear_speculation ();
      return true;
    }
}

/* Assume that both THIS and a given context is valid and strenghten THIS
   if possible.  Return true if any strenghtening was made.
   If actual type the context is being used in is known, OTR_TYPE should be
   set accordingly. This improves quality of combined result.  */

bool
ipa_polymorphic_call_context::combine_with (ipa_polymorphic_call_context ctx,
					    tree otr_type)
{
  bool updated = false;

  if (ctx.useless_p () || invalid)
    return false;

  /* Restricting context to inner type makes merging easier, however do not
     do that unless we know how the context is used (OTR_TYPE is non-NULL)  */
  if (otr_type && !invalid && !ctx.invalid)
    {
      restrict_to_inner_class (otr_type);
      ctx.restrict_to_inner_class (otr_type);
      if(invalid)
        return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Polymorphic call context combine:");
      dump (dump_file);
      fprintf (dump_file, "With context:                    ");
      ctx.dump (dump_file);
      if (otr_type)
	{
          fprintf (dump_file, "To be used with type:            ");
	  print_generic_expr (dump_file, otr_type, TDF_SLIM);
          fprintf (dump_file, "\n");
	}
    }

  /* If call is known to be invalid, we are done.  */
  if (ctx.invalid)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "-> Invalid context\n");
      goto invalidate;
    }

  if (!ctx.outer_type)
    ;
  else if (!outer_type)
    {
      outer_type = ctx.outer_type;
      offset = ctx.offset;
      dynamic = ctx.dynamic;
      maybe_in_construction = ctx.maybe_in_construction;
      maybe_derived_type = ctx.maybe_derived_type;
      updated = true;
    }
  /* If types are known to be same, merging is quite easy.  */
  else if (types_must_be_same_for_odr (outer_type, ctx.outer_type))
    {
      if (offset != ctx.offset
	  && TYPE_SIZE (outer_type)
	  && TREE_CODE (TYPE_SIZE (outer_type)) == INTEGER_CST)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Outer types match, offset mismatch -> invalid\n");
	  clear_speculation ();
	  clear_outer_type ();
	  invalid = true;
	  return true;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "Outer types match, merging flags\n");
      if (maybe_in_construction && !ctx.maybe_in_construction)
	{
	  updated = true;
	  maybe_in_construction = false;
	}
      if (maybe_derived_type && !ctx.maybe_derived_type)
	{
	  updated = true;
	  maybe_derived_type = false;
	}
      if (dynamic && !ctx.dynamic)
	{
	  updated = true;
	  dynamic = false;
	}
    }
  /* If we know the type precisely, there is not much to improve.  */
  else if (!maybe_derived_type && !maybe_in_construction
	   && !ctx.maybe_derived_type && !ctx.maybe_in_construction)
    {
      /* It may be easy to check if second context permits the first
	 and set INVALID otherwise.  This is not easy to do in general;
	 contains_type_p may return false negatives for non-comparable
	 types.  

	 If OTR_TYPE is known, we however can expect that
	 restrict_to_inner_class should have discovered the same base
	 type.  */
      if (otr_type && !ctx.maybe_in_construction && !ctx.maybe_derived_type)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Contextes disagree -> invalid\n");
	  goto invalidate;
	}
    }
  /* See if one type contains the other as a field (not base).
     In this case we want to choose the wider type, because it contains
     more information.  */
  else if (contains_type_p (ctx.outer_type, ctx.offset - offset,
			    outer_type, false, false))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Second type contain the first as a field\n");

      if (maybe_derived_type)
	{
	  outer_type = ctx.outer_type;
	  maybe_derived_type = ctx.maybe_derived_type;
	  offset = ctx.offset;
	  dynamic = ctx.dynamic;
	  updated = true;
	}

      /* If we do not know how the context is being used, we can
	 not clear MAYBE_IN_CONSTRUCTION because it may be offseted
	 to other component of OUTER_TYPE later and we know nothing
	 about it.  */
      if (otr_type && maybe_in_construction
	  && !ctx.maybe_in_construction)
	{
          maybe_in_construction = false;
	  updated = true;
	}
    }
  else if (contains_type_p (outer_type, offset - ctx.offset,
			    ctx.outer_type, false, false))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "First type contain the second as a field\n");

      if (otr_type && maybe_in_construction
	  && !ctx.maybe_in_construction)
	{
          maybe_in_construction = false;
	  updated = true;
	}
    }
  /* See if OUTER_TYPE is base of CTX.OUTER_TYPE.  */
  else if (contains_type_p (ctx.outer_type,
			    ctx.offset - offset, outer_type, false, true))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "First type is base of second\n");
      if (!maybe_derived_type)
	{
	  if (!ctx.maybe_in_construction
	      && types_odr_comparable (outer_type, ctx.outer_type))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Second context does not permit base -> invalid\n");
	      goto invalidate;
	    }
	}
      /* Pick variant deeper in the hiearchy.  */
      else
	{
	  outer_type = ctx.outer_type;
	  maybe_in_construction = ctx.maybe_in_construction;
	  maybe_derived_type = ctx.maybe_derived_type;
	  offset = ctx.offset;
	  dynamic = ctx.dynamic;
          updated = true;
	}
    }
  /* See if CTX.OUTER_TYPE is base of OUTER_TYPE.  */
  else if (contains_type_p (outer_type,
			    offset - ctx.offset, ctx.outer_type, false, true))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Second type is base of first\n");
      if (!ctx.maybe_derived_type)
	{
	  if (!maybe_in_construction
	      && types_odr_comparable (outer_type, ctx.outer_type))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "First context does not permit base -> invalid\n");
	      goto invalidate;
	    }
	  /* Pick the base type.  */
	  else if (maybe_in_construction)
	    {
	      outer_type = ctx.outer_type;
	      maybe_in_construction = ctx.maybe_in_construction;
	      maybe_derived_type = ctx.maybe_derived_type;
	      offset = ctx.offset;
	      dynamic = ctx.dynamic;
	      updated = true;
	    }
	}
    }
  /* TODO handle merging using hiearchy. */
  else if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Giving up on merge\n");

  updated |= combine_speculation_with (ctx.speculative_outer_type,
				       ctx.speculative_offset,
				       ctx.speculative_maybe_derived_type,
				       otr_type);

  if (updated && dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Updated as:                      ");
      dump (dump_file);
      fprintf (dump_file, "\n");
    }
  return updated;

invalidate:
  invalid = true;
  clear_speculation ();
  clear_outer_type ();
  return true;
}

/* Take non-speculative info, merge it with speculative and clear speculation.
   Used when we no longer manage to keep track of actual outer type, but we
   think it is still there.

   If OTR_TYPE is set, the transformation can be done more effectively assuming
   that context is going to be used only that way.  */

void
ipa_polymorphic_call_context::make_speculative (tree otr_type)
{
  tree spec_outer_type = outer_type;
  HOST_WIDE_INT spec_offset = offset;
  bool spec_maybe_derived_type = maybe_derived_type;

  if (invalid)
    {
      invalid = false;
      clear_outer_type ();
      clear_speculation ();
      return;
    }
  if (!outer_type)
    return;
  clear_outer_type ();
  combine_speculation_with (spec_outer_type, spec_offset,
			    spec_maybe_derived_type,
			    otr_type);
}

/* Use when we can not track dynamic type change.  This speculatively assume
   type change is not happening.  */

void
ipa_polymorphic_call_context::possible_dynamic_type_change (bool in_poly_cdtor,
							    tree otr_type)
{
  if (dynamic)
    make_speculative (otr_type);
  else if (in_poly_cdtor)
    maybe_in_construction = true;
}

/* Return TRUE if this context conveys the same information as OTHER.  */

bool
ipa_polymorphic_call_context::equal_to
    (const ipa_polymorphic_call_context &x) const
{
  if (useless_p ())
    return x.useless_p ();
  if (invalid)
    return x.invalid;
  if (x.useless_p () || x.invalid)
    return false;

  if (outer_type)
    {
      if (!x.outer_type
	  || !types_odr_comparable (outer_type, x.outer_type)
	  || !types_same_for_odr (outer_type, x.outer_type)
	  || offset != x.offset
	  || maybe_in_construction != x.maybe_in_construction
	  || maybe_derived_type != x.maybe_derived_type
	  || dynamic != x.dynamic)
	return false;
    }
  else if (x.outer_type)
    return false;


  if (speculative_outer_type
      && speculation_consistent_p (speculative_outer_type, speculative_offset,
				   speculative_maybe_derived_type, NULL_TREE))
    {
      if (!x.speculative_outer_type)
	return false;

      if (!types_odr_comparable (speculative_outer_type,
				 x.speculative_outer_type)
	  || !types_same_for_odr  (speculative_outer_type,
				   x.speculative_outer_type)
	  || speculative_offset != x.speculative_offset
	  || speculative_maybe_derived_type != x.speculative_maybe_derived_type)
	return false;
    }
  else if (x.speculative_outer_type
	   && x.speculation_consistent_p (x.speculative_outer_type,
					  x.speculative_offset,
				  	  x.speculative_maybe_derived_type,
					  NULL))
    return false;

  return true;
}

/* Modify context to be strictly less restrictive than CTX.  */

bool
ipa_polymorphic_call_context::meet_with (ipa_polymorphic_call_context ctx,
					 tree otr_type)
{
  bool updated = false;

  if (useless_p () || ctx.invalid)
    return false;

  /* Restricting context to inner type makes merging easier, however do not
     do that unless we know how the context is used (OTR_TYPE is non-NULL)  */
  if (otr_type && !useless_p () && !ctx.useless_p ())
    {
      restrict_to_inner_class (otr_type);
      ctx.restrict_to_inner_class (otr_type);
      if(invalid)
        return false;
    }

  if (equal_to (ctx))
    return false;

  if (ctx.useless_p () || invalid)
    {
      *this = ctx;
      return true;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Polymorphic call context meet:");
      dump (dump_file);
      fprintf (dump_file, "With context:                    ");
      ctx.dump (dump_file);
      if (otr_type)
	{
          fprintf (dump_file, "To be used with type:            ");
	  print_generic_expr (dump_file, otr_type, TDF_SLIM);
          fprintf (dump_file, "\n");
	}
    }

  if (!dynamic && ctx.dynamic)
    {
      dynamic = true;
      updated = true;
    }

  /* If call is known to be invalid, we are done.  */
  if (!outer_type)
    ;
  else if (!ctx.outer_type)
    {
      clear_outer_type ();
      updated = true;
    }
  /* If types are known to be same, merging is quite easy.  */
  else if (types_must_be_same_for_odr (outer_type, ctx.outer_type))
    {
      if (offset != ctx.offset
	  && TYPE_SIZE (outer_type)
	  && TREE_CODE (TYPE_SIZE (outer_type)) == INTEGER_CST)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Outer types match, offset mismatch -> clearing\n");
	  clear_outer_type ();
	  return true;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "Outer types match, merging flags\n");
      if (!maybe_in_construction && ctx.maybe_in_construction)
	{
	  updated = true;
	  maybe_in_construction = true;
	}
      if (!maybe_derived_type && ctx.maybe_derived_type)
	{
	  updated = true;
	  maybe_derived_type = true;
	}
      if (!dynamic && ctx.dynamic)
	{
	  updated = true;
	  dynamic = true;
	}
    }
  /* See if one type contains the other as a field (not base).  */
  else if (contains_type_p (ctx.outer_type, ctx.offset - offset,
			    outer_type, false, false))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Second type contain the first as a field\n");

      /* The second type is more specified, so we keep the first.
         We need to set DYNAMIC flag to avoid declaring context INVALID
	 of OFFSET ends up being out of range.  */
      if (!dynamic
	  && (ctx.dynamic
	      || (!otr_type
		  && (!TYPE_SIZE (ctx.outer_type)
		      || !TYPE_SIZE (outer_type)
		      || !operand_equal_p (TYPE_SIZE (ctx.outer_type),
					   TYPE_SIZE (outer_type), 0)))))
	{
	  dynamic = true;
	  updated = true;
	}
    }
  else if (contains_type_p (outer_type, offset - ctx.offset,
			    ctx.outer_type, false, false))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "First type contain the second as a field\n");

      if (!dynamic
	  && (ctx.dynamic
	      || (!otr_type
		  && (!TYPE_SIZE (ctx.outer_type)
		      || !TYPE_SIZE (outer_type)
		      || !operand_equal_p (TYPE_SIZE (ctx.outer_type),
					   TYPE_SIZE (outer_type), 0)))))
	dynamic = true;
      outer_type = ctx.outer_type;
      offset = ctx.offset;
      dynamic = ctx.dynamic;
      maybe_in_construction = ctx.maybe_in_construction;
      maybe_derived_type = ctx.maybe_derived_type;
      updated = true;
    }
  /* See if OUTER_TYPE is base of CTX.OUTER_TYPE.  */
  else if (contains_type_p (ctx.outer_type,
			    ctx.offset - offset, outer_type, false, true))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "First type is base of second\n");
      if (!maybe_derived_type)
	{
	  maybe_derived_type = true;
	  updated = true;
	}
      if (!maybe_in_construction && ctx.maybe_in_construction)
	{
	  maybe_in_construction = true;
	  updated = true;
	}
      if (!dynamic && ctx.dynamic)
	{
	  dynamic = true;
	  updated = true;
	}
    }
  /* See if CTX.OUTER_TYPE is base of OUTER_TYPE.  */
  else if (contains_type_p (outer_type,
			    offset - ctx.offset, ctx.outer_type, false, true))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Second type is base of first\n");
      outer_type = ctx.outer_type;
      offset = ctx.offset;
      updated = true;
      if (!maybe_derived_type)
	maybe_derived_type = true;
      if (!maybe_in_construction && ctx.maybe_in_construction)
	maybe_in_construction = true;
      if (!dynamic && ctx.dynamic)
	dynamic = true;
    }
  /* TODO handle merging using hiearchy. */
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "Giving up on meet\n");
      clear_outer_type ();
      updated = true;
    }

  updated |= meet_speculation_with (ctx.speculative_outer_type,
				    ctx.speculative_offset,
				    ctx.speculative_maybe_derived_type,
				    otr_type);

  if (updated && dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Updated as:                      ");
      dump (dump_file);
      fprintf (dump_file, "\n");
    }
  return updated;
}
