/* Subroutines used to generate function calls and handle built-in
   instructions on IBM RS/6000.
   Copyright (C) 1991-2023 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "ira.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic-core.h"
#include "insn-attr.h"
#include "flags.h"
#include "alias.h"
#include "fold-const.h"
#include "attribs.h"
#include "stor-layout.h"
#include "calls.h"
#include "print-tree.h"
#include "varasm.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "ssa.h"
#include "tree-ssa-propagate.h"
#include "builtins.h"
#include "tree-vector-builder.h"
#include "ppc-auxv.h"
#include "targhooks.h"
#include "opts.h"

#include "rs6000-internal.h"

#ifndef TARGET_PROFILE_KERNEL
#define TARGET_PROFILE_KERNEL 0
#endif

#ifdef HAVE_AS_GNU_ATTRIBUTE
# ifndef HAVE_LD_PPC_GNU_ATTR_LONG_DOUBLE
# define HAVE_LD_PPC_GNU_ATTR_LONG_DOUBLE 0
# endif
#endif

#ifndef TARGET_NO_PROTOTYPE
#define TARGET_NO_PROTOTYPE 0
#endif

/* Nonzero if we can use a floating-point register to pass this arg.  */
#define USE_FP_FOR_ARG_P(CUM,MODE)		\
  (SCALAR_FLOAT_MODE_NOT_VECTOR_P (MODE)		\
   && (CUM)->fregno <= FP_ARG_MAX_REG		\
   && TARGET_HARD_FLOAT)

/* Nonzero if we can use an AltiVec register to pass this arg.  */
#define USE_ALTIVEC_FOR_ARG_P(CUM,MODE,NAMED)			\
  (ALTIVEC_OR_VSX_VECTOR_MODE (MODE)				\
   && (CUM)->vregno <= ALTIVEC_ARG_MAX_REG			\
   && TARGET_ALTIVEC_ABI					\
   && (NAMED))

/* Walk down the type tree of TYPE counting consecutive base elements.
   If *MODEP is VOIDmode, then set it to the first valid floating point
   or vector type.  If a non-floating point or vector type is found, or
   if a floating point or vector type that doesn't match a non-VOIDmode
   *MODEP is found, then return -1, otherwise return the count in the
   sub-tree.

   There have been some ABI snafus along the way with C++.  Modify
   EMPTY_BASE_SEEN to a nonzero value iff a C++ empty base class makes
   an appearance; separate flag bits indicate whether or not such a
   field is marked "no unique address".  Modify ZERO_WIDTH_BF_SEEN
   to 1 iff a C++ zero-length bitfield makes an appearance, but
   in this case otherwise treat this as still being a homogeneous
   aggregate.  */

static int
rs6000_aggregate_candidate (const_tree type, machine_mode *modep,
			    int *empty_base_seen, int *zero_width_bf_seen)
{
  machine_mode mode;
  HOST_WIDE_INT size;

  switch (TREE_CODE (type))
    {
    case REAL_TYPE:
      mode = TYPE_MODE (type);
      if (!SCALAR_FLOAT_MODE_P (mode))
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 1;

      break;

    case COMPLEX_TYPE:
      mode = TYPE_MODE (TREE_TYPE (type));
      if (!SCALAR_FLOAT_MODE_P (mode))
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 2;

      break;

    case VECTOR_TYPE:
      if (!TARGET_ALTIVEC_ABI || !TARGET_ALTIVEC)
	return -1;

      /* Use V4SImode as representative of all 128-bit vector types.  */
      size = int_size_in_bytes (type);
      switch (size)
	{
	case 16:
	  mode = V4SImode;
	  break;
	default:
	  return -1;
	}

      if (*modep == VOIDmode)
	*modep = mode;

      /* Vector modes are considered to be opaque: two vectors are
	 equivalent for the purposes of being homogeneous aggregates
	 if they are the same size.  */
      if (*modep == mode)
	return 1;

      break;

    case ARRAY_TYPE:
      {
	int count;
	tree index = TYPE_DOMAIN (type);

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	count = rs6000_aggregate_candidate (TREE_TYPE (type), modep,
					    empty_base_seen,
					    zero_width_bf_seen);
	if (count == -1
	    || !index
	    || !TYPE_MAX_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MAX_VALUE (index))
	    || !TYPE_MIN_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MIN_VALUE (index))
	    || count < 0)
	  return -1;

	count *= (1 + tree_to_uhwi (TYPE_MAX_VALUE (index))
		      - tree_to_uhwi (TYPE_MIN_VALUE (index)));

	/* There must be no padding.  */
	if (wi::to_wide (TYPE_SIZE (type))
	    != count * GET_MODE_BITSIZE (*modep))
	  return -1;

	return count;
      }

    case RECORD_TYPE:
      {
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    if (DECL_FIELD_CXX_ZERO_WIDTH_BIT_FIELD (field))
	      {
		/* GCC 11 and earlier generated incorrect code in a rare
		   corner case for C++.  When a RECORD_TYPE looks like a
		   homogeneous aggregate, except that it also contains
		   one or more zero-width bit fields, these earlier
		   compilers would incorrectly pass the fields in FPRs
		   or VSRs.  This occurred because the front end wrongly
		   removed these bitfields from the RECORD_TYPE.  In
		   GCC 12 and later, the front end flaw was corrected.
		   We want to diagnose this case.  To do this, we pretend
		   that we don't see the zero-width bit fields (hence
		   the continue statement here), but pass back a flag
		   indicating what happened.  The caller then diagnoses
		   the issue and rejects the RECORD_TYPE as a homogeneous
		   aggregate.  */
		*zero_width_bf_seen = 1;
		continue;
	      }

	    if (DECL_FIELD_ABI_IGNORED (field))
	      {
		if (lookup_attribute ("no_unique_address",
				      DECL_ATTRIBUTES (field)))
		  *empty_base_seen |= 2;
		else
		  *empty_base_seen |= 1;
		continue;
	      }

	    sub_count = rs6000_aggregate_candidate (TREE_TYPE (field), modep,
						    empty_base_seen,
						    zero_width_bf_seen);
	    if (sub_count < 0)
	      return -1;
	    count += sub_count;
	  }

	/* There must be no padding.  */
	if (wi::to_wide (TYPE_SIZE (type))
	    != count * GET_MODE_BITSIZE (*modep))
	  return -1;

	return count;
      }

    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	/* These aren't very interesting except in a degenerate case.  */
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    sub_count = rs6000_aggregate_candidate (TREE_TYPE (field), modep,
						    empty_base_seen,
						    zero_width_bf_seen);
	    if (sub_count < 0)
	      return -1;
	    count = count > sub_count ? count : sub_count;
	  }

	/* There must be no padding.  */
	if (wi::to_wide (TYPE_SIZE (type))
	    != count * GET_MODE_BITSIZE (*modep))
	  return -1;

	return count;
      }

    default:
      break;
    }

  return -1;
}

/* If an argument, whose type is described by TYPE and MODE, is a homogeneous
   float or vector aggregate that shall be passed in FP/vector registers
   according to the ELFv2 ABI, return the homogeneous element mode in
   *ELT_MODE and the number of elements in *N_ELTS, and return TRUE.

   Otherwise, set *ELT_MODE to MODE and *N_ELTS to 1, and return FALSE.  */

bool
rs6000_discover_homogeneous_aggregate (machine_mode mode, const_tree type,
				       machine_mode *elt_mode,
				       int *n_elts)
{
  /* Note that we do not accept complex types at the top level as
     homogeneous aggregates; these types are handled via the
     targetm.calls.split_complex_arg mechanism.  Complex types
     can be elements of homogeneous aggregates, however.  */
  if (TARGET_HARD_FLOAT && DEFAULT_ABI == ABI_ELFv2 && type
      && AGGREGATE_TYPE_P (type))
    {
      machine_mode field_mode = VOIDmode;
      int empty_base_seen = 0;
      int zero_width_bf_seen = 0;
      int field_count = rs6000_aggregate_candidate (type, &field_mode,
						    &empty_base_seen,
						    &zero_width_bf_seen);

      if (field_count > 0)
	{
	  int reg_size = ALTIVEC_OR_VSX_VECTOR_MODE (field_mode) ? 16 : 8;
	  int field_size = ROUND_UP (GET_MODE_SIZE (field_mode), reg_size);

	  /* The ELFv2 ABI allows homogeneous aggregates to occupy
	     up to AGGR_ARG_NUM_REG registers.  */
	  if (field_count * field_size <= AGGR_ARG_NUM_REG * reg_size)
	    {
	      if (elt_mode)
		*elt_mode = field_mode;
	      if (n_elts)
		*n_elts = field_count;
	      if (empty_base_seen && warn_psabi)
		{
		  static unsigned last_reported_type_uid;
		  unsigned uid = TYPE_UID (TYPE_MAIN_VARIANT (type));
		  if (uid != last_reported_type_uid)
		    {
		      const char *url
			= CHANGES_ROOT_URL "gcc-10/changes.html#empty_base";
		      if (empty_base_seen & 1)
			inform (input_location,
				"parameter passing for argument of type %qT "
				"when C++17 is enabled changed to match C++14 "
				"%{in GCC 10.1%}", type, url);
		      else
			inform (input_location,
				"parameter passing for argument of type %qT "
				"with %<[[no_unique_address]]%> members "
				"changed %{in GCC 10.1%}", type, url);
		      last_reported_type_uid = uid;
		    }
		}
	      if (zero_width_bf_seen && warn_psabi)
		{
		  static unsigned last_reported_type_uid;
		  unsigned uid = TYPE_UID (TYPE_MAIN_VARIANT (type));
		  if (uid != last_reported_type_uid)
		    {
		      inform (input_location,
			      "ELFv2 parameter passing for an argument "
			      "containing zero-width bit fields but that is "
			      "otherwise a homogeneous aggregate was "
			      "corrected in GCC 12");
		      last_reported_type_uid = uid;
		    }
		  if (elt_mode)
		    *elt_mode = mode;
		  if (n_elts)
		    *n_elts = 1;
		  return false;
		}
	      return true;
	    }
	}
    }

  if (elt_mode)
    *elt_mode = mode;
  if (n_elts)
    *n_elts = 1;
  return false;
}

/* Return a nonzero value to say to return the function value in
   memory, just as large structures are always returned.  TYPE will be
   the data type of the value, and FNTYPE will be the type of the
   function doing the returning, or @code{NULL} for libcalls.

   The AIX ABI for the RS/6000 specifies that all structures are
   returned in memory.  The Darwin ABI does the same.
   
   For the Darwin 64 Bit ABI, a function result can be returned in
   registers or in memory, depending on the size of the return data
   type.  If it is returned in registers, the value occupies the same
   registers as it would if it were the first and only function
   argument.  Otherwise, the function places its result in memory at
   the location pointed to by GPR3.
   
   The SVR4 ABI specifies that structures <= 8 bytes are returned in r3/r4, 
   but a draft put them in memory, and GCC used to implement the draft
   instead of the final standard.  Therefore, aix_struct_return
   controls this instead of DEFAULT_ABI; V.4 targets needing backward
   compatibility can change DRAFT_V4_STRUCT_RET to override the
   default, and -m switches get the final word.  See
   rs6000_option_override_internal for more details.

   The PPC32 SVR4 ABI uses IEEE double extended for long double, if 128-bit
   long double support is enabled.  These values are returned in memory.

   int_size_in_bytes returns -1 for variable size objects, which go in
   memory always.  The cast to unsigned makes -1 > 8.  */

bool
rs6000_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  /* We do not allow MMA types being used as return values.  Only report
     the invalid return value usage the first time we encounter it.  */
  if (cfun
      && !cfun->machine->mma_return_type_error
      && TREE_TYPE (cfun->decl) == fntype
      && (TYPE_MODE (type) == OOmode || TYPE_MODE (type) == XOmode))
    {
      /* Record we have now handled function CFUN, so the next time we
	 are called, we do not re-report the same error.  */
      cfun->machine->mma_return_type_error = true;
      if (TYPE_CANONICAL (type) != NULL_TREE)
	type = TYPE_CANONICAL (type);
      error ("invalid use of MMA type %qs as a function return value",
	     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))));
    }

  /* For the Darwin64 ABI, test if we can fit the return value in regs.  */
  if (TARGET_MACHO
      && rs6000_darwin64_abi
      && TREE_CODE (type) == RECORD_TYPE
      && int_size_in_bytes (type) > 0)
    {
      CUMULATIVE_ARGS valcum;
      rtx valret;

      valcum.words = 0;
      valcum.fregno = FP_ARG_MIN_REG;
      valcum.vregno = ALTIVEC_ARG_MIN_REG;
      /* Do a trial code generation as if this were going to be passed
	 as an argument; if any part goes in memory, we return NULL.  */
      valret = rs6000_darwin64_record_arg (&valcum, type, true, true);
      if (valret)
	return false;
      /* Otherwise fall through to more conventional ABI rules.  */
    }

  /* The ELFv2 ABI returns homogeneous VFP aggregates in registers */
  if (rs6000_discover_homogeneous_aggregate (TYPE_MODE (type), type,
					     NULL, NULL))
    return false;

  /* The ELFv2 ABI returns aggregates up to 16B in registers */
  if (DEFAULT_ABI == ABI_ELFv2 && AGGREGATE_TYPE_P (type)
      && (unsigned HOST_WIDE_INT) int_size_in_bytes (type) <= 16)
    return false;

  if (AGGREGATE_TYPE_P (type)
      && (aix_struct_return
	  || (unsigned HOST_WIDE_INT) int_size_in_bytes (type) > 8))
    return true;

  /* Allow -maltivec -mabi=no-altivec without warning.  Altivec vector
     modes only exist for GCC vector types if -maltivec.  */
  if (TARGET_32BIT && !TARGET_ALTIVEC_ABI
      && ALTIVEC_VECTOR_MODE (TYPE_MODE (type)))
    return false;

  /* Return synthetic vectors in memory.  */
  if (TREE_CODE (type) == VECTOR_TYPE
      && int_size_in_bytes (type) > (TARGET_ALTIVEC_ABI ? 16 : 8))
    {
      static bool warned_for_return_big_vectors = false;
      if (!warned_for_return_big_vectors)
	{
	  warning (OPT_Wpsabi, "GCC vector returned by reference: "
		   "non-standard ABI extension with no compatibility "
		   "guarantee");
	  warned_for_return_big_vectors = true;
	}
      return true;
    }

  if (DEFAULT_ABI == ABI_V4 && TARGET_IEEEQUAD
      && FLOAT128_IEEE_P (TYPE_MODE (type)))
    return true;

  return false;
}

/* Specify whether values returned in registers should be at the most
   significant end of a register.  We want aggregates returned by
   value to match the way aggregates are passed to functions.  */

bool
rs6000_return_in_msb (const_tree valtype)
{
  return (DEFAULT_ABI == ABI_ELFv2
	  && BYTES_BIG_ENDIAN
	  && AGGREGATE_TYPE_P (valtype)
	  && (rs6000_function_arg_padding (TYPE_MODE (valtype), valtype)
	      == PAD_UPWARD));
}

#ifdef HAVE_AS_GNU_ATTRIBUTE
/* Return TRUE if a call to function FNDECL may be one that
   potentially affects the function calling ABI of the object file.  */

static bool
call_ABI_of_interest (tree fndecl)
{
  if (rs6000_gnu_attr && symtab->state == EXPANSION)
    {
      struct cgraph_node *c_node;

      /* Libcalls are always interesting.  */
      if (fndecl == NULL_TREE)
	return true;

      /* Any call to an external function is interesting.  */
      if (DECL_EXTERNAL (fndecl))
	return true;

      /* Interesting functions that we are emitting in this object file.  */
      c_node = cgraph_node::get (fndecl);
      c_node = c_node->ultimate_alias_target ();
      return !c_node->only_called_directly_p ();
    }
  return false;
}
#endif

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0 and RETURN_MODE the return value mode.

   For incoming args we set the number of arguments in the prototype large
   so we never return a PARALLEL.  */

void
init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype,
		      rtx libname ATTRIBUTE_UNUSED, int incoming,
		      int libcall, int n_named_args,
		      tree fndecl,
		      machine_mode return_mode ATTRIBUTE_UNUSED)
{
  static CUMULATIVE_ARGS zero_cumulative;

  *cum = zero_cumulative;
  cum->words = 0;
  cum->fregno = FP_ARG_MIN_REG;
  cum->vregno = ALTIVEC_ARG_MIN_REG;
  cum->prototype = (fntype && prototype_p (fntype));
  cum->call_cookie = ((DEFAULT_ABI == ABI_V4 && libcall)
		      ? CALL_LIBCALL : CALL_NORMAL);
  cum->sysv_gregno = GP_ARG_MIN_REG;
  cum->stdarg = stdarg_p (fntype);
  cum->libcall = libcall;

  cum->nargs_prototype = 0;
  if (incoming || cum->prototype)
    cum->nargs_prototype = n_named_args;

  /* Check for a longcall attribute.  */
  if ((!fntype && rs6000_default_long_calls)
      || (fntype
	  && lookup_attribute ("longcall", TYPE_ATTRIBUTES (fntype))
	  && !lookup_attribute ("shortcall", TYPE_ATTRIBUTES (fntype))))
    cum->call_cookie |= CALL_LONG;
  else if (DEFAULT_ABI != ABI_DARWIN)
    {
      bool is_local = (fndecl
		       && !DECL_EXTERNAL (fndecl)
		       && !DECL_WEAK (fndecl)
		       && (*targetm.binds_local_p) (fndecl));
      if (is_local)
	;
      else if (flag_plt)
	{
	  if (fntype
	      && lookup_attribute ("noplt", TYPE_ATTRIBUTES (fntype)))
	    cum->call_cookie |= CALL_LONG;
	}
      else
	{
	  if (!(fntype
		&& lookup_attribute ("plt", TYPE_ATTRIBUTES (fntype))))
	    cum->call_cookie |= CALL_LONG;
	}
    }

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "\ninit_cumulative_args:");
      if (fntype)
	{
	  tree ret_type = TREE_TYPE (fntype);
	  fprintf (stderr, " ret code = %s,",
		   get_tree_code_name (TREE_CODE (ret_type)));
	}

      if (cum->call_cookie & CALL_LONG)
	fprintf (stderr, " longcall,");

      fprintf (stderr, " proto = %d, nargs = %d\n",
	       cum->prototype, cum->nargs_prototype);
    }

#ifdef HAVE_AS_GNU_ATTRIBUTE
  if (TARGET_ELF && (TARGET_64BIT || DEFAULT_ABI == ABI_V4))
    {
      cum->escapes = call_ABI_of_interest (fndecl);
      if (cum->escapes)
	{
	  tree return_type;

	  if (fntype)
	    {
	      return_type = TREE_TYPE (fntype);
	      return_mode = TYPE_MODE (return_type);
	    }
	  else
	    return_type = lang_hooks.types.type_for_mode (return_mode, 0);

	  if (return_type != NULL)
	    {
	      if (TREE_CODE (return_type) == RECORD_TYPE
		  && TYPE_TRANSPARENT_AGGR (return_type))
		{
		  return_type = TREE_TYPE (first_field (return_type));
		  return_mode = TYPE_MODE (return_type);
		}
	      if (AGGREGATE_TYPE_P (return_type)
		  && ((unsigned HOST_WIDE_INT) int_size_in_bytes (return_type)
		      <= 8))
		rs6000_returns_struct = true;
	    }
	  if (SCALAR_FLOAT_MODE_P (return_mode))
	    {
	      rs6000_passes_float = true;
	      if ((HAVE_LD_PPC_GNU_ATTR_LONG_DOUBLE || TARGET_64BIT)
		  && (FLOAT128_IBM_P (return_mode)
		      || FLOAT128_IEEE_P (return_mode)
		      || (return_type != NULL
			  && (TYPE_MAIN_VARIANT (return_type)
			      == long_double_type_node))))
		rs6000_passes_long_double = true;
	    }
	  if (ALTIVEC_OR_VSX_VECTOR_MODE (return_mode))
	    rs6000_passes_vector = true;
	}
    }
#endif

  if (fntype
      && !TARGET_ALTIVEC
      && TARGET_ALTIVEC_ABI
      && ALTIVEC_VECTOR_MODE (TYPE_MODE (TREE_TYPE (fntype))))
    {
      error ("cannot return value in vector register because"
	     " altivec instructions are disabled, use %qs"
	     " to enable them", "-maltivec");
    }
}


/* On rs6000, function arguments are promoted, as are function return
   values.  */

machine_mode
rs6000_promote_function_mode (const_tree type ATTRIBUTE_UNUSED,
			      machine_mode mode,
			      int *punsignedp ATTRIBUTE_UNUSED,
			      const_tree, int for_return ATTRIBUTE_UNUSED)
{
  if (GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_SIZE (mode) < (TARGET_32BIT ? 4 : 8))
    mode = TARGET_32BIT ? SImode : DImode;

  return mode;
}

/* Return true if TYPE must be passed on the stack and not in registers.  */

bool
rs6000_must_pass_in_stack (const function_arg_info &arg)
{
  if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2 || TARGET_64BIT)
    return must_pass_in_stack_var_size (arg);
  else
    return must_pass_in_stack_var_size_or_pad (arg);
}

static inline bool
is_complex_IBM_long_double (machine_mode mode)
{
  return mode == ICmode || (mode == TCmode && FLOAT128_IBM_P (TCmode));
}

/* Whether ABI_V4 passes MODE args to a function in floating point
   registers.  */

static bool
abi_v4_pass_in_fpr (machine_mode mode, bool named)
{
  if (!TARGET_HARD_FLOAT)
    return false;
  if (mode == DFmode)
    return true;
  if (mode == SFmode && named)
    return true;
  /* ABI_V4 passes complex IBM long double in 8 gprs.
     Stupid, but we can't change the ABI now.  */
  if (is_complex_IBM_long_double (mode))
    return false;
  if (FLOAT128_2REG_P (mode))
    return true;
  if (DECIMAL_FLOAT_MODE_P (mode))
    return true;
  return false;
}

/* Implement TARGET_FUNCTION_ARG_PADDING.

   For the AIX ABI structs are always stored left shifted in their
   argument slot.  */

pad_direction
rs6000_function_arg_padding (machine_mode mode, const_tree type)
{
#ifndef AGGREGATE_PADDING_FIXED
#define AGGREGATE_PADDING_FIXED 0
#endif
#ifndef AGGREGATES_PAD_UPWARD_ALWAYS
#define AGGREGATES_PAD_UPWARD_ALWAYS 0
#endif

  if (!AGGREGATE_PADDING_FIXED)
    {
      /* GCC used to pass structures of the same size as integer types as
	 if they were in fact integers, ignoring TARGET_FUNCTION_ARG_PADDING.
	 i.e. Structures of size 1 or 2 (or 4 when TARGET_64BIT) were
	 passed padded downward, except that -mstrict-align further
	 muddied the water in that multi-component structures of 2 and 4
	 bytes in size were passed padded upward.

	 The following arranges for best compatibility with previous
	 versions of gcc, but removes the -mstrict-align dependency.  */
      if (BYTES_BIG_ENDIAN)
	{
	  HOST_WIDE_INT size = 0;

	  if (mode == BLKmode)
	    {
	      if (type && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
		size = int_size_in_bytes (type);
	    }
	  else
	    size = GET_MODE_SIZE (mode);

	  if (size == 1 || size == 2 || size == 4)
	    return PAD_DOWNWARD;
	}
      return PAD_UPWARD;
    }

  if (AGGREGATES_PAD_UPWARD_ALWAYS)
    {
      if (type != 0 && AGGREGATE_TYPE_P (type))
	return PAD_UPWARD;
    }

  /* Fall back to the default.  */
  return default_function_arg_padding (mode, type);
}

/* If defined, a C expression that gives the alignment boundary, in bits,
   of an argument with the specified mode and type.  If it is not defined,
   PARM_BOUNDARY is used for all arguments.

   V.4 wants long longs and doubles to be double word aligned.  Just
   testing the mode size is a boneheaded way to do this as it means
   that other types such as complex int are also double word aligned.
   However, we're stuck with this because changing the ABI might break
   existing library interfaces.

   Quadword align Altivec/VSX vectors.
   Quadword align large synthetic vector types.   */

unsigned int
rs6000_function_arg_boundary (machine_mode mode, const_tree type)
{
  machine_mode elt_mode;
  int n_elts;

  rs6000_discover_homogeneous_aggregate (mode, type, &elt_mode, &n_elts);

  if (DEFAULT_ABI == ABI_V4
      && (GET_MODE_SIZE (mode) == 8
	  || (TARGET_HARD_FLOAT
	      && !is_complex_IBM_long_double (mode)
	      && FLOAT128_2REG_P (mode))))
    return 64;
  else if (FLOAT128_VECTOR_P (mode))
    return 128;
  else if (type && TREE_CODE (type) == VECTOR_TYPE
	   && int_size_in_bytes (type) >= 8
	   && int_size_in_bytes (type) < 16)
    return 64;
  else if (ALTIVEC_OR_VSX_VECTOR_MODE (elt_mode)
	   || (type && TREE_CODE (type) == VECTOR_TYPE
	       && int_size_in_bytes (type) >= 16))
    return 128;

  /* Aggregate types that need > 8 byte alignment are quadword-aligned
     in the parameter area in the ELFv2 ABI, and in the AIX ABI unless
     -mcompat-align-parm is used.  */
  if (((DEFAULT_ABI == ABI_AIX && !rs6000_compat_align_parm)
       || DEFAULT_ABI == ABI_ELFv2)
      && type && TYPE_ALIGN (type) > 64)
    {
      /* "Aggregate" means any AGGREGATE_TYPE except for single-element
         or homogeneous float/vector aggregates here.  We already handled
         vector aggregates above, but still need to check for float here. */
      if (AGGREGATE_TYPE_P (type)
	  && !SCALAR_FLOAT_MODE_P (elt_mode))
	return 128;
    }

  /* Similar for the Darwin64 ABI.  Note that for historical reasons we
     implement the "aggregate type" check as a BLKmode check here; this
     means certain aggregate types are in fact not aligned.  */
  if (TARGET_MACHO && rs6000_darwin64_abi
      && mode == BLKmode
      && type && TYPE_ALIGN (type) > 64)
    return 128;

  return PARM_BOUNDARY;
}

/* The offset in words to the start of the parameter save area.  */

static unsigned int
rs6000_parm_offset (void)
{
  return (DEFAULT_ABI == ABI_V4 ? 2
	  : DEFAULT_ABI == ABI_ELFv2 ? 4
	  : 6);
}

/* For a function parm of MODE and TYPE, return the starting word in
   the parameter area.  NWORDS of the parameter area are already used.  */

static unsigned int
rs6000_parm_start (machine_mode mode, const_tree type,
		   unsigned int nwords)
{
  unsigned int align;

  align = rs6000_function_arg_boundary (mode, type) / PARM_BOUNDARY - 1;
  return nwords + (-(rs6000_parm_offset () + nwords) & align);
}

/* Compute the size (in words) of a function argument.  */

static unsigned long
rs6000_arg_size (machine_mode mode, const_tree type)
{
  unsigned long size;

  if (mode != BLKmode)
    size = GET_MODE_SIZE (mode);
  else
    size = int_size_in_bytes (type);

  if (TARGET_32BIT)
    return (size + 3) >> 2;
  else
    return (size + 7) >> 3;
}

/* Use this to flush pending int fields.  */

static void
rs6000_darwin64_record_arg_advance_flush (CUMULATIVE_ARGS *cum,
					  HOST_WIDE_INT bitpos, int final)
{
  unsigned int startbit, endbit;
  int intregs, intoffset;

  /* Handle the situations where a float is taking up the first half
     of the GPR, and the other half is empty (typically due to
     alignment restrictions). We can detect this by a 8-byte-aligned
     int field, or by seeing that this is the final flush for this
     argument. Count the word and continue on.  */
  if (cum->floats_in_gpr == 1
      && (cum->intoffset % 64 == 0
	  || (cum->intoffset == -1 && final)))
    {
      cum->words++;
      cum->floats_in_gpr = 0;
    }

  if (cum->intoffset == -1)
    return;

  intoffset = cum->intoffset;
  cum->intoffset = -1;
  cum->floats_in_gpr = 0;

  if (intoffset % BITS_PER_WORD != 0)
    {
      unsigned int bits = BITS_PER_WORD - intoffset % BITS_PER_WORD;
      if (!int_mode_for_size (bits, 0).exists ())
	{
	  /* We couldn't find an appropriate mode, which happens,
	     e.g., in packed structs when there are 3 bytes to load.
	     Back intoffset back to the beginning of the word in this
	     case.  */
	  intoffset = ROUND_DOWN (intoffset, BITS_PER_WORD);
	}
    }

  startbit = ROUND_DOWN (intoffset, BITS_PER_WORD);
  endbit = ROUND_UP (bitpos, BITS_PER_WORD);
  intregs = (endbit - startbit) / BITS_PER_WORD;
  cum->words += intregs;
  /* words should be unsigned. */
  if ((unsigned)cum->words < (endbit/BITS_PER_WORD))
    {
      int pad = (endbit/BITS_PER_WORD) - cum->words;
      cum->words += pad;
    }
}

/* The darwin64 ABI calls for us to recurse down through structs,
   looking for elements passed in registers.  Unfortunately, we have
   to track int register count here also because of misalignments
   in powerpc alignment mode.  */

static void
rs6000_darwin64_record_arg_advance_recurse (CUMULATIVE_ARGS *cum,
					    const_tree type,
					    HOST_WIDE_INT startbitpos)
{
  tree f;

  for (f = TYPE_FIELDS (type); f ; f = DECL_CHAIN (f))
    if (TREE_CODE (f) == FIELD_DECL)
      {
	HOST_WIDE_INT bitpos = startbitpos;
	tree ftype = TREE_TYPE (f);
	machine_mode mode;
	if (ftype == error_mark_node)
	  continue;
	mode = TYPE_MODE (ftype);

	if (DECL_SIZE (f) != 0
	    && tree_fits_uhwi_p (bit_position (f)))
	  bitpos += int_bit_position (f);

	/* ??? FIXME: else assume zero offset.  */

	if (TREE_CODE (ftype) == RECORD_TYPE)
	  rs6000_darwin64_record_arg_advance_recurse (cum, ftype, bitpos);
	else if (USE_FP_FOR_ARG_P (cum, mode))
	  {
	    unsigned n_fpregs = (GET_MODE_SIZE (mode) + 7) >> 3;
	    rs6000_darwin64_record_arg_advance_flush (cum, bitpos, 0);
	    cum->fregno += n_fpregs;
	    /* Single-precision floats present a special problem for
	       us, because they are smaller than an 8-byte GPR, and so
	       the structure-packing rules combined with the standard
	       varargs behavior mean that we want to pack float/float
	       and float/int combinations into a single register's
	       space. This is complicated by the arg advance flushing,
	       which works on arbitrarily large groups of int-type
	       fields.  */
	    if (mode == SFmode)
	      {
		if (cum->floats_in_gpr == 1)
		  {
		    /* Two floats in a word; count the word and reset
		       the float count.  */
		    cum->words++;
		    cum->floats_in_gpr = 0;
		  }
		else if (bitpos % 64 == 0)
		  {
		    /* A float at the beginning of an 8-byte word;
		       count it and put off adjusting cum->words until
		       we see if a arg advance flush is going to do it
		       for us.  */
		    cum->floats_in_gpr++;
		  }
		else
		  {
		    /* The float is at the end of a word, preceded
		       by integer fields, so the arg advance flush
		       just above has already set cum->words and
		       everything is taken care of.  */
		  }
	      }
	    else
	      cum->words += n_fpregs;
	  }
	else if (USE_ALTIVEC_FOR_ARG_P (cum, mode, 1))
	  {
	    rs6000_darwin64_record_arg_advance_flush (cum, bitpos, 0);
	    cum->vregno++;
	    cum->words += 2;
	  }
	else if (cum->intoffset == -1)
	  cum->intoffset = bitpos;
      }
}

/* Check for an item that needs to be considered specially under the darwin 64
   bit ABI.  These are record types where the mode is BLK or the structure is
   8 bytes in size.  */
int
rs6000_darwin64_struct_check_p (machine_mode mode, const_tree type)
{
  return rs6000_darwin64_abi
	 && ((mode == BLKmode 
	      && TREE_CODE (type) == RECORD_TYPE 
	      && int_size_in_bytes (type) > 0)
	  || (type && TREE_CODE (type) == RECORD_TYPE 
	      && int_size_in_bytes (type) == 8)) ? 1 : 0;
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)

   Note that for args passed by reference, function_arg will be called
   with MODE and TYPE set to that of the pointer to the arg, not the arg
   itself.  */

static void
rs6000_function_arg_advance_1 (CUMULATIVE_ARGS *cum, machine_mode mode,
			       const_tree type, bool named, int depth)
{
  machine_mode elt_mode;
  int n_elts;

  rs6000_discover_homogeneous_aggregate (mode, type, &elt_mode, &n_elts);

  /* Only tick off an argument if we're not recursing.  */
  if (depth == 0)
    cum->nargs_prototype--;

#ifdef HAVE_AS_GNU_ATTRIBUTE
  if (TARGET_ELF && (TARGET_64BIT || DEFAULT_ABI == ABI_V4)
      && cum->escapes)
    {
      if (SCALAR_FLOAT_MODE_P (mode))
	{
	  rs6000_passes_float = true;
	  if ((HAVE_LD_PPC_GNU_ATTR_LONG_DOUBLE || TARGET_64BIT)
	      && (FLOAT128_IBM_P (mode)
		  || FLOAT128_IEEE_P (mode)
		  || (type != NULL
		      && TYPE_MAIN_VARIANT (type) == long_double_type_node)))
	    rs6000_passes_long_double = true;
	}
      if (named && ALTIVEC_OR_VSX_VECTOR_MODE (mode))
	rs6000_passes_vector = true;
    }
#endif

  if (TARGET_ALTIVEC_ABI
      && (ALTIVEC_OR_VSX_VECTOR_MODE (elt_mode)
	  || (type && TREE_CODE (type) == VECTOR_TYPE
	      && int_size_in_bytes (type) == 16)))
    {
      bool stack = false;

      if (USE_ALTIVEC_FOR_ARG_P (cum, elt_mode, named))
	{
	  cum->vregno += n_elts;

	  /* If we are not splitting Complex IEEE128 args then account for the
	     fact that they are passed in 2 VSX regs. */
	  if (!targetm.calls.split_complex_arg && type
	      && TREE_CODE (type) == COMPLEX_TYPE && elt_mode == KCmode)
	    cum->vregno++;

	  if (!TARGET_ALTIVEC)
	    error ("cannot pass argument in vector register because"
		   " altivec instructions are disabled, use %qs"
		   " to enable them", "-maltivec");

	  /* PowerPC64 Linux and AIX allocate GPRs for a vector argument
	     even if it is going to be passed in a vector register.
	     Darwin does the same for variable-argument functions.  */
	  if (((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
	       && TARGET_64BIT)
	      || (cum->stdarg && DEFAULT_ABI != ABI_V4))
	    stack = true;
	}
      else
	stack = true;

      if (stack)
	{
	  int align;

	  /* Vector parameters must be 16-byte aligned.  In 32-bit
	     mode this means we need to take into account the offset
	     to the parameter save area.  In 64-bit mode, they just
	     have to start on an even word, since the parameter save
	     area is 16-byte aligned.  */
	  if (TARGET_32BIT)
	    align = -(rs6000_parm_offset () + cum->words) & 3;
	  else
	    align = cum->words & 1;
	  cum->words += align + rs6000_arg_size (mode, type);

	  if (TARGET_DEBUG_ARG)
	    {
	      fprintf (stderr, "function_adv: words = %2d, align=%d, ",
		       cum->words, align);
	      fprintf (stderr, "nargs = %4d, proto = %d, mode = %4s\n",
		       cum->nargs_prototype, cum->prototype,
		       GET_MODE_NAME (mode));
	    }
	}
    }
  else if (TARGET_MACHO && rs6000_darwin64_struct_check_p (mode, type))
    {
      int size = int_size_in_bytes (type);
      /* Variable sized types have size == -1 and are
	 treated as if consisting entirely of ints.
	 Pad to 16 byte boundary if needed.  */
      if (TYPE_ALIGN (type) >= 2 * BITS_PER_WORD
	  && (cum->words % 2) != 0)
	cum->words++;
      /* For varargs, we can just go up by the size of the struct. */
      if (!named)
	cum->words += (size + 7) / 8;
      else
	{
	  /* It is tempting to say int register count just goes up by
	     sizeof(type)/8, but this is wrong in a case such as
	     { int; double; int; } [powerpc alignment].  We have to
	     grovel through the fields for these too.  */
	  cum->intoffset = 0;
	  cum->floats_in_gpr = 0;
	  rs6000_darwin64_record_arg_advance_recurse (cum, type, 0);
	  rs6000_darwin64_record_arg_advance_flush (cum,
						    size * BITS_PER_UNIT, 1);
	}
	  if (TARGET_DEBUG_ARG)
	    {
	      fprintf (stderr, "function_adv: words = %2d, align=%d, size=%d",
		       cum->words, TYPE_ALIGN (type), size);
	      fprintf (stderr, 
	           "nargs = %4d, proto = %d, mode = %4s (darwin64 abi)\n",
		       cum->nargs_prototype, cum->prototype,
		       GET_MODE_NAME (mode));
	    }
    }
  else if (DEFAULT_ABI == ABI_V4)
    {
      if (abi_v4_pass_in_fpr (mode, named))
	{
	  /* _Decimal128 must use an even/odd register pair.  This assumes
	     that the register number is odd when fregno is odd.  */
	  if (mode == TDmode && (cum->fregno % 2) == 1)
	    cum->fregno++;

	  if (cum->fregno + (FLOAT128_2REG_P (mode) ? 1 : 0)
	      <= FP_ARG_V4_MAX_REG)
	    cum->fregno += (GET_MODE_SIZE (mode) + 7) >> 3;
	  else
	    {
	      cum->fregno = FP_ARG_V4_MAX_REG + 1;
	      if (mode == DFmode || FLOAT128_IBM_P (mode)
		  || mode == DDmode || mode == TDmode)
		cum->words += cum->words & 1;
	      cum->words += rs6000_arg_size (mode, type);
	    }
	}
      else
	{
	  int n_words = rs6000_arg_size (mode, type);
	  int gregno = cum->sysv_gregno;

	  /* Long long is put in (r3,r4), (r5,r6), (r7,r8) or (r9,r10).
	     As does any other 2 word item such as complex int due to a
	     historical mistake.  */
	  if (n_words == 2)
	    gregno += (1 - gregno) & 1;

	  /* Multi-reg args are not split between registers and stack.  */
	  if (gregno + n_words - 1 > GP_ARG_MAX_REG)
	    {
	      /* Long long is aligned on the stack.  So are other 2 word
		 items such as complex int due to a historical mistake.  */
	      if (n_words == 2)
		cum->words += cum->words & 1;
	      cum->words += n_words;
	    }

	  /* Note: continuing to accumulate gregno past when we've started
	     spilling to the stack indicates the fact that we've started
	     spilling to the stack to expand_builtin_saveregs.  */
	  cum->sysv_gregno = gregno + n_words;
	}

      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "function_adv: words = %2d, fregno = %2d, ",
		   cum->words, cum->fregno);
	  fprintf (stderr, "gregno = %2d, nargs = %4d, proto = %d, ",
		   cum->sysv_gregno, cum->nargs_prototype, cum->prototype);
	  fprintf (stderr, "mode = %4s, named = %d\n",
		   GET_MODE_NAME (mode), named);
	}
    }
  else
    {
      int n_words = rs6000_arg_size (mode, type);
      int start_words = cum->words;
      int align_words = rs6000_parm_start (mode, type, start_words);

      cum->words = align_words + n_words;

      if (SCALAR_FLOAT_MODE_P (elt_mode) && TARGET_HARD_FLOAT)
	{
	  /* _Decimal128 must be passed in an even/odd float register pair.
	     This assumes that the register number is odd when fregno is
	     odd.  */
	  if (elt_mode == TDmode && (cum->fregno % 2) == 1)
	    cum->fregno++;
	  cum->fregno += n_elts * ((GET_MODE_SIZE (elt_mode) + 7) >> 3);
	}

      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "function_adv: words = %2d, fregno = %2d, ",
		   cum->words, cum->fregno);
	  fprintf (stderr, "nargs = %4d, proto = %d, mode = %4s, ",
		   cum->nargs_prototype, cum->prototype, GET_MODE_NAME (mode));
	  fprintf (stderr, "named = %d, align = %d, depth = %d\n",
		   named, align_words - start_words, depth);
	}
    }
}

void
rs6000_function_arg_advance (cumulative_args_t cum,
			     const function_arg_info &arg)
{
  rs6000_function_arg_advance_1 (get_cumulative_args (cum),
				 arg.mode, arg.type, arg.named, 0);
}

/* A subroutine of rs6000_darwin64_record_arg.  Assign the bits of the
   structure between cum->intoffset and bitpos to integer registers.  */

static void
rs6000_darwin64_record_arg_flush (CUMULATIVE_ARGS *cum,
				  HOST_WIDE_INT bitpos, rtx rvec[], int *k)
{
  machine_mode mode;
  unsigned int regno;
  unsigned int startbit, endbit;
  int this_regno, intregs, intoffset;
  rtx reg;

  if (cum->intoffset == -1)
    return;

  intoffset = cum->intoffset;
  cum->intoffset = -1;

  /* If this is the trailing part of a word, try to only load that
     much into the register.  Otherwise load the whole register.  Note
     that in the latter case we may pick up unwanted bits.  It's not a
     problem at the moment but may wish to revisit.  */

  if (intoffset % BITS_PER_WORD != 0)
    {
      unsigned int bits = BITS_PER_WORD - intoffset % BITS_PER_WORD;
      if (!int_mode_for_size (bits, 0).exists (&mode))
	{
	  /* We couldn't find an appropriate mode, which happens,
	     e.g., in packed structs when there are 3 bytes to load.
	     Back intoffset back to the beginning of the word in this
	     case.  */
	  intoffset = ROUND_DOWN (intoffset, BITS_PER_WORD);
	  mode = word_mode;
	}
    }
  else
    mode = word_mode;

  startbit = ROUND_DOWN (intoffset, BITS_PER_WORD);
  endbit = ROUND_UP (bitpos, BITS_PER_WORD);
  intregs = (endbit - startbit) / BITS_PER_WORD;
  this_regno = cum->words + intoffset / BITS_PER_WORD;

  if (intregs > 0 && intregs > GP_ARG_NUM_REG - this_regno)
    cum->use_stack = 1;

  intregs = MIN (intregs, GP_ARG_NUM_REG - this_regno);
  if (intregs <= 0)
    return;

  intoffset /= BITS_PER_UNIT;
  do
    {
      regno = GP_ARG_MIN_REG + this_regno;
      reg = gen_rtx_REG (mode, regno);
      rvec[(*k)++] =
	gen_rtx_EXPR_LIST (VOIDmode, reg, GEN_INT (intoffset));

      this_regno += 1;
      intoffset = (intoffset | (UNITS_PER_WORD-1)) + 1;
      mode = word_mode;
      intregs -= 1;
    }
  while (intregs > 0);
}

/* Recursive workhorse for the following.  */

static void
rs6000_darwin64_record_arg_recurse (CUMULATIVE_ARGS *cum, const_tree type,
				    HOST_WIDE_INT startbitpos, rtx rvec[],
				    int *k)
{
  tree f;

  for (f = TYPE_FIELDS (type); f ; f = DECL_CHAIN (f))
    if (TREE_CODE (f) == FIELD_DECL)
      {
	HOST_WIDE_INT bitpos = startbitpos;
	tree ftype = TREE_TYPE (f);
	machine_mode mode;
	if (ftype == error_mark_node)
	  continue;
	mode = TYPE_MODE (ftype);

	if (DECL_SIZE (f) != 0
	    && tree_fits_uhwi_p (bit_position (f)))
	  bitpos += int_bit_position (f);

	/* ??? FIXME: else assume zero offset.  */

	if (TREE_CODE (ftype) == RECORD_TYPE)
	  rs6000_darwin64_record_arg_recurse (cum, ftype, bitpos, rvec, k);
	else if (cum->named && USE_FP_FOR_ARG_P (cum, mode))
	  {
	    unsigned n_fpreg = (GET_MODE_SIZE (mode) + 7) >> 3;
#if 0
	    switch (mode)
	      {
	      case E_SCmode: mode = SFmode; break;
	      case E_DCmode: mode = DFmode; break;
	      case E_TCmode: mode = TFmode; break;
	      default: break;
	      }
#endif
	    rs6000_darwin64_record_arg_flush (cum, bitpos, rvec, k);
	    if (cum->fregno + n_fpreg > FP_ARG_MAX_REG + 1)
	      {
		gcc_assert (cum->fregno == FP_ARG_MAX_REG
			    && (mode == TFmode || mode == TDmode));
		/* Long double or _Decimal128 split over regs and memory.  */
		mode = DECIMAL_FLOAT_MODE_P (mode) ? DDmode : DFmode;
		cum->use_stack=1;
	      }
	    rvec[(*k)++]
	      = gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (mode, cum->fregno++),
				   GEN_INT (bitpos / BITS_PER_UNIT));
	    if (FLOAT128_2REG_P (mode))
	      cum->fregno++;
	  }
	else if (cum->named && USE_ALTIVEC_FOR_ARG_P (cum, mode, 1))
	  {
	    rs6000_darwin64_record_arg_flush (cum, bitpos, rvec, k);
	    rvec[(*k)++]
	      = gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (mode, cum->vregno++),
				   GEN_INT (bitpos / BITS_PER_UNIT));
	  }
	else if (cum->intoffset == -1)
	  cum->intoffset = bitpos;
      }
}

/* For the darwin64 ABI, we want to construct a PARALLEL consisting of
   the register(s) to be used for each field and subfield of a struct
   being passed by value, along with the offset of where the
   register's value may be found in the block.  FP fields go in FP
   register, vector fields go in vector registers, and everything
   else goes in int registers, packed as in memory.

   This code is also used for function return values.  RETVAL indicates
   whether this is the case.

   Much of this is taken from the SPARC V9 port, which has a similar
   calling convention.  */

rtx
rs6000_darwin64_record_arg (CUMULATIVE_ARGS *orig_cum, const_tree type,
			    bool named, bool retval)
{
  rtx rvec[FIRST_PSEUDO_REGISTER];
  int k = 1, kbase = 1;
  HOST_WIDE_INT typesize = int_size_in_bytes (type);
  /* This is a copy; modifications are not visible to our caller.  */
  CUMULATIVE_ARGS copy_cum = *orig_cum;
  CUMULATIVE_ARGS *cum = &copy_cum;

  /* Pad to 16 byte boundary if needed.  */
  if (!retval && TYPE_ALIGN (type) >= 2 * BITS_PER_WORD
      && (cum->words % 2) != 0)
    cum->words++;

  cum->intoffset = 0;
  cum->use_stack = 0;
  cum->named = named;

  /* Put entries into rvec[] for individual FP and vector fields, and
     for the chunks of memory that go in int regs.  Note we start at
     element 1; 0 is reserved for an indication of using memory, and
     may or may not be filled in below. */
  rs6000_darwin64_record_arg_recurse (cum, type, /* startbit pos= */ 0, rvec, &k);
  rs6000_darwin64_record_arg_flush (cum, typesize * BITS_PER_UNIT, rvec, &k);

  /* If any part of the struct went on the stack put all of it there.
     This hack is because the generic code for
     FUNCTION_ARG_PARTIAL_NREGS cannot handle cases where the register
     parts of the struct are not at the beginning.  */
  if (cum->use_stack)
    {
      if (retval)
	return NULL_RTX;    /* doesn't go in registers at all */
      kbase = 0;
      rvec[0] = gen_rtx_EXPR_LIST (VOIDmode, NULL_RTX, const0_rtx);
    }
  if (k > 1 || cum->use_stack)
    return gen_rtx_PARALLEL (BLKmode, gen_rtvec_v (k - kbase, &rvec[kbase]));
  else
    return NULL_RTX;
}

/* Determine where to place an argument in 64-bit mode with 32-bit ABI.  */

static rtx
rs6000_mixed_function_arg (machine_mode mode, const_tree type,
			   int align_words)
{
  int n_units;
  int i, k;
  rtx rvec[GP_ARG_NUM_REG + 1];

  if (align_words >= GP_ARG_NUM_REG)
    return NULL_RTX;

  n_units = rs6000_arg_size (mode, type);

  /* Optimize the simple case where the arg fits in one gpr, except in
     the case of BLKmode due to assign_parms assuming that registers are
     BITS_PER_WORD wide.  */
  if (n_units == 0
      || (n_units == 1 && mode != BLKmode))
    return gen_rtx_REG (mode, GP_ARG_MIN_REG + align_words);

  k = 0;
  if (align_words + n_units > GP_ARG_NUM_REG)
    /* Not all of the arg fits in gprs.  Say that it goes in memory too,
       using a magic NULL_RTX component.
       This is not strictly correct.  Only some of the arg belongs in
       memory, not all of it.  However, the normal scheme using
       function_arg_partial_nregs can result in unusual subregs, eg.
       (subreg:SI (reg:DF) 4), which are not handled well.  The code to
       store the whole arg to memory is often more efficient than code
       to store pieces, and we know that space is available in the right
       place for the whole arg.  */
    rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, NULL_RTX, const0_rtx);

  i = 0;
  do
    {
      rtx r = gen_rtx_REG (SImode, GP_ARG_MIN_REG + align_words);
      rtx off = GEN_INT (i++ * 4);
      rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, r, off);
    }
  while (++align_words < GP_ARG_NUM_REG && --n_units != 0);

  return gen_rtx_PARALLEL (mode, gen_rtvec_v (k, rvec));
}

/* We have an argument of MODE and TYPE that goes into FPRs or VRs,
   but must also be copied into the parameter save area starting at
   offset ALIGN_WORDS.  Fill in RVEC with the elements corresponding
   to the GPRs and/or memory.  Return the number of elements used.  */

static int
rs6000_psave_function_arg (machine_mode mode, const_tree type,
			   int align_words, rtx *rvec)
{
  int k = 0;

  if (align_words < GP_ARG_NUM_REG)
    {
      int n_words = rs6000_arg_size (mode, type);

      if (align_words + n_words > GP_ARG_NUM_REG
	  || mode == BLKmode
	  || (TARGET_32BIT && TARGET_POWERPC64))
	{
	  /* If this is partially on the stack, then we only
	     include the portion actually in registers here.  */
	  machine_mode rmode = TARGET_32BIT ? SImode : DImode;
	  int i = 0;

	  if (align_words + n_words > GP_ARG_NUM_REG)
	    {
	      /* Not all of the arg fits in gprs.  Say that it goes in memory
		 too, using a magic NULL_RTX component.  Also see comment in
		 rs6000_mixed_function_arg for why the normal
		 function_arg_partial_nregs scheme doesn't work in this case. */
	      rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, NULL_RTX, const0_rtx);
	    }

	  do
	    {
	      rtx r = gen_rtx_REG (rmode, GP_ARG_MIN_REG + align_words);
	      rtx off = GEN_INT (i++ * GET_MODE_SIZE (rmode));
	      rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, r, off);
	    }
	  while (++align_words < GP_ARG_NUM_REG && --n_words != 0);
	}
      else
	{
	  /* The whole arg fits in gprs.  */
	  rtx r = gen_rtx_REG (mode, GP_ARG_MIN_REG + align_words);
	  rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, r, const0_rtx);
	}
    }
  else
    {
      /* It's entirely in memory.  */
      rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, NULL_RTX, const0_rtx);
    }

  return k;
}

/* RVEC is a vector of K components of an argument of mode MODE.
   Construct the final function_arg return value from it.  */

static rtx
rs6000_finish_function_arg (machine_mode mode, rtx *rvec, int k)
{
  gcc_assert (k >= 1);

  /* Avoid returning a PARALLEL in the trivial cases.  */
  if (k == 1)
    {
      if (XEXP (rvec[0], 0) == NULL_RTX)
	return NULL_RTX;

      if (GET_MODE (XEXP (rvec[0], 0)) == mode)
	return XEXP (rvec[0], 0);
    }

  return gen_rtx_PARALLEL (mode, gen_rtvec_v (k, rvec));
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.  It is
    not modified in this routine.
   ARG is a description of the argument.

   On RS/6000 the first eight words of non-FP are normally in registers
   and the rest are pushed.  Under AIX, the first 13 FP args are in registers.
   Under V.4, the first 8 FP args are in registers.

   If this is floating-point and no prototype is specified, we use
   both an FP and integer register (or possibly FP reg and stack).  Library
   functions (when CALL_LIBCALL is set) always have the proper types for args,
   so we can pass the FP value just in one register.  emit_library_function
   doesn't support PARALLEL anyway.

   Note that for args passed by reference, function_arg will be called
   with ARG describing the pointer to the arg, not the arg itself.  */

rtx
rs6000_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  tree type = arg.type;
  machine_mode mode = arg.mode;
  bool named = arg.named;
  enum rs6000_abi abi = DEFAULT_ABI;
  machine_mode elt_mode;
  int n_elts;

  /* We do not allow MMA types being used as function arguments.  */
  if (mode == OOmode || mode == XOmode)
    {
      if (TYPE_CANONICAL (type) != NULL_TREE)
	type = TYPE_CANONICAL (type);
      error ("invalid use of MMA operand of type %qs as a function parameter",
	     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))));
      return NULL_RTX;
    }

  /* Return a marker to indicate whether CR1 needs to set or clear the
     bit that V.4 uses to say fp args were passed in registers.
     Assume that we don't need the marker for software floating point,
     or compiler generated library calls.  */
  if (arg.end_marker_p ())
    {
      if (abi == ABI_V4
	  && (cum->call_cookie & CALL_LIBCALL) == 0
	  && (cum->stdarg
	      || (cum->nargs_prototype < 0
		  && (cum->prototype || TARGET_NO_PROTOTYPE)))
	  && TARGET_HARD_FLOAT)
	return GEN_INT (cum->call_cookie
			| ((cum->fregno == FP_ARG_MIN_REG)
			   ? CALL_V4_SET_FP_ARGS
			   : CALL_V4_CLEAR_FP_ARGS));

      return GEN_INT (cum->call_cookie & ~CALL_LIBCALL);
    }

  rs6000_discover_homogeneous_aggregate (mode, type, &elt_mode, &n_elts);

  if (TARGET_MACHO && rs6000_darwin64_struct_check_p (mode, type))
    {
      rtx rslt = rs6000_darwin64_record_arg (cum, type, named, /*retval= */false);
      if (rslt != NULL_RTX)
	return rslt;
      /* Else fall through to usual handling.  */
    }

  if (USE_ALTIVEC_FOR_ARG_P (cum, elt_mode, named))
    {
      rtx rvec[GP_ARG_NUM_REG + AGGR_ARG_NUM_REG + 1];
      rtx r, off;
      int i, k = 0;

      /* Do we also need to pass this argument in the parameter save area?
	 Library support functions for IEEE 128-bit are assumed to not need the
	 value passed both in GPRs and in vector registers.  */
      if (TARGET_64BIT && !cum->prototype
	  && (!cum->libcall || !FLOAT128_VECTOR_P (elt_mode)))
	{
	  int align_words = ROUND_UP (cum->words, 2);
	  k = rs6000_psave_function_arg (mode, type, align_words, rvec);
	}

      /* Describe where this argument goes in the vector registers.  */
      for (i = 0; i < n_elts && cum->vregno + i <= ALTIVEC_ARG_MAX_REG; i++)
	{
	  r = gen_rtx_REG (elt_mode, cum->vregno + i);
	  off = GEN_INT (i * GET_MODE_SIZE (elt_mode));
	  rvec[k++] =  gen_rtx_EXPR_LIST (VOIDmode, r, off);
	}

      return rs6000_finish_function_arg (mode, rvec, k);
    }
  else if (TARGET_ALTIVEC_ABI
	   && (ALTIVEC_OR_VSX_VECTOR_MODE (mode)
	       || (type && TREE_CODE (type) == VECTOR_TYPE
		   && int_size_in_bytes (type) == 16)))
    {
      if (named || abi == ABI_V4)
	return NULL_RTX;
      else
	{
	  /* Vector parameters to varargs functions under AIX or Darwin
	     get passed in memory and possibly also in GPRs.  */
	  int align, align_words, n_words;
	  machine_mode part_mode;

	  /* Vector parameters must be 16-byte aligned.  In 32-bit
	     mode this means we need to take into account the offset
	     to the parameter save area.  In 64-bit mode, they just
	     have to start on an even word, since the parameter save
	     area is 16-byte aligned.  */
	  if (TARGET_32BIT)
	    align = -(rs6000_parm_offset () + cum->words) & 3;
	  else
	    align = cum->words & 1;
	  align_words = cum->words + align;

	  /* Out of registers?  Memory, then.  */
	  if (align_words >= GP_ARG_NUM_REG)
	    return NULL_RTX;

	  if (TARGET_32BIT && TARGET_POWERPC64)
	    return rs6000_mixed_function_arg (mode, type, align_words);

	  /* The vector value goes in GPRs.  Only the part of the
	     value in GPRs is reported here.  */
	  part_mode = mode;
	  n_words = rs6000_arg_size (mode, type);
	  if (align_words + n_words > GP_ARG_NUM_REG)
	    /* Fortunately, there are only two possibilities, the value
	       is either wholly in GPRs or half in GPRs and half not.  */
	    part_mode = DImode;

	  return gen_rtx_REG (part_mode, GP_ARG_MIN_REG + align_words);
	}
    }

  else if (abi == ABI_V4)
    {
      if (abi_v4_pass_in_fpr (mode, named))
	{
	  /* _Decimal128 must use an even/odd register pair.  This assumes
	     that the register number is odd when fregno is odd.  */
	  if (mode == TDmode && (cum->fregno % 2) == 1)
	    cum->fregno++;

	  if (cum->fregno + (FLOAT128_2REG_P (mode) ? 1 : 0)
	      <= FP_ARG_V4_MAX_REG)
	    return gen_rtx_REG (mode, cum->fregno);
	  else
	    return NULL_RTX;
	}
      else
	{
	  int n_words = rs6000_arg_size (mode, type);
	  int gregno = cum->sysv_gregno;

	  /* Long long is put in (r3,r4), (r5,r6), (r7,r8) or (r9,r10).
	     As does any other 2 word item such as complex int due to a
	     historical mistake.  */
	  if (n_words == 2)
	    gregno += (1 - gregno) & 1;

	  /* Multi-reg args are not split between registers and stack.  */
	  if (gregno + n_words - 1 > GP_ARG_MAX_REG)
	    return NULL_RTX;

	  if (TARGET_32BIT && TARGET_POWERPC64)
	    return rs6000_mixed_function_arg (mode, type,
					      gregno - GP_ARG_MIN_REG);
	  return gen_rtx_REG (mode, gregno);
	}
    }
  else
    {
      int align_words = rs6000_parm_start (mode, type, cum->words);

      /* _Decimal128 must be passed in an even/odd float register pair.
	 This assumes that the register number is odd when fregno is odd.  */
      if (elt_mode == TDmode && (cum->fregno % 2) == 1)
	cum->fregno++;

      if (USE_FP_FOR_ARG_P (cum, elt_mode)
	  && !(TARGET_AIX && !TARGET_ELF
	       && type != NULL && AGGREGATE_TYPE_P (type)))
	{
	  rtx rvec[GP_ARG_NUM_REG + AGGR_ARG_NUM_REG + 1];
	  rtx r, off;
	  int i, k = 0;
	  unsigned long n_fpreg = (GET_MODE_SIZE (elt_mode) + 7) >> 3;
	  int fpr_words;

	  /* Do we also need to pass this argument in the parameter
	     save area?  */
	  if (type && (cum->nargs_prototype <= 0
		       || ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
			   && TARGET_XL_COMPAT
			   && align_words >= GP_ARG_NUM_REG)))
	    k = rs6000_psave_function_arg (mode, type, align_words, rvec);

	  /* Describe where this argument goes in the fprs.  */
	  for (i = 0; i < n_elts
		      && cum->fregno + i * n_fpreg <= FP_ARG_MAX_REG; i++)
	    {
	      /* Check if the argument is split over registers and memory.
		 This can only ever happen for long double or _Decimal128;
		 complex types are handled via split_complex_arg.  */
	      machine_mode fmode = elt_mode;
	      if (cum->fregno + (i + 1) * n_fpreg > FP_ARG_MAX_REG + 1)
		{
		  gcc_assert (FLOAT128_2REG_P (fmode));
		  fmode = DECIMAL_FLOAT_MODE_P (fmode) ? DDmode : DFmode;
		}

	      r = gen_rtx_REG (fmode, cum->fregno + i * n_fpreg);
	      off = GEN_INT (i * GET_MODE_SIZE (elt_mode));
	      rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, r, off);
	    }

	  /* If there were not enough FPRs to hold the argument, the rest
	     usually goes into memory.  However, if the current position
	     is still within the register parameter area, a portion may
	     actually have to go into GPRs.

	     Note that it may happen that the portion of the argument
	     passed in the first "half" of the first GPR was already
	     passed in the last FPR as well.

	     For unnamed arguments, we already set up GPRs to cover the
	     whole argument in rs6000_psave_function_arg, so there is
	     nothing further to do at this point.  */
	  fpr_words = (i * GET_MODE_SIZE (elt_mode)) / (TARGET_32BIT ? 4 : 8);
	  if (i < n_elts && align_words + fpr_words < GP_ARG_NUM_REG
	      && cum->nargs_prototype > 0)
            {
	      machine_mode rmode = TARGET_32BIT ? SImode : DImode;
	      int n_words = rs6000_arg_size (mode, type);

	      align_words += fpr_words;
	      n_words -= fpr_words;

	      do
		{
		  r = gen_rtx_REG (rmode, GP_ARG_MIN_REG + align_words);
		  off = GEN_INT (fpr_words++ * GET_MODE_SIZE (rmode));
		  rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, r, off);
		}
	      while (++align_words < GP_ARG_NUM_REG && --n_words != 0);
	    }

	  return rs6000_finish_function_arg (mode, rvec, k);
	}
      else if (align_words < GP_ARG_NUM_REG)
	{
	  if (TARGET_32BIT && TARGET_POWERPC64)
	    return rs6000_mixed_function_arg (mode, type, align_words);

	  return gen_rtx_REG (mode, GP_ARG_MIN_REG + align_words);
	}
      else
	return NULL_RTX;
    }
}

/* For an arg passed partly in registers and partly in memory, this is
   the number of bytes passed in registers.  For args passed entirely in
   registers or entirely in memory, zero.  When an arg is described by a
   PARALLEL, perhaps using more than one register type, this function
   returns the number of bytes used by the first element of the PARALLEL.  */

int
rs6000_arg_partial_bytes (cumulative_args_t cum_v,
			  const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  bool passed_in_gprs = true;
  int ret = 0;
  int align_words;
  machine_mode elt_mode;
  int n_elts;

  rs6000_discover_homogeneous_aggregate (arg.mode, arg.type,
					 &elt_mode, &n_elts);

  if (DEFAULT_ABI == ABI_V4)
    return 0;

  if (USE_ALTIVEC_FOR_ARG_P (cum, elt_mode, arg.named))
    {
      /* If we are passing this arg in the fixed parameter save area (gprs or
         memory) as well as VRs, we do not use the partial bytes mechanism;
         instead, rs6000_function_arg will return a PARALLEL including a memory
         element as necessary.  Library support functions for IEEE 128-bit are
         assumed to not need the value passed both in GPRs and in vector
         registers.  */
      if (TARGET_64BIT && !cum->prototype
	  && (!cum->libcall || !FLOAT128_VECTOR_P (elt_mode)))
	return 0;

      /* Otherwise, we pass in VRs only.  Check for partial copies.  */
      passed_in_gprs = false;
      if (cum->vregno + n_elts > ALTIVEC_ARG_MAX_REG + 1)
	ret = (ALTIVEC_ARG_MAX_REG + 1 - cum->vregno) * 16;
    }

  /* In this complicated case we just disable the partial_nregs code.  */
  if (TARGET_MACHO && rs6000_darwin64_struct_check_p (arg.mode, arg.type))
    return 0;

  align_words = rs6000_parm_start (arg.mode, arg.type, cum->words);

  if (USE_FP_FOR_ARG_P (cum, elt_mode)
      && !(TARGET_AIX && !TARGET_ELF && arg.aggregate_type_p ()))
    {
      unsigned long n_fpreg = (GET_MODE_SIZE (elt_mode) + 7) >> 3;

      /* If we are passing this arg in the fixed parameter save area
         (gprs or memory) as well as FPRs, we do not use the partial
	 bytes mechanism; instead, rs6000_function_arg will return a
	 PARALLEL including a memory element as necessary.  */
      if (arg.type
	  && (cum->nargs_prototype <= 0
	      || ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
		  && TARGET_XL_COMPAT
		  && align_words >= GP_ARG_NUM_REG)))
	return 0;

      /* Otherwise, we pass in FPRs only.  Check for partial copies.  */
      passed_in_gprs = false;
      if (cum->fregno + n_elts * n_fpreg > FP_ARG_MAX_REG + 1)
	{
	  /* Compute number of bytes / words passed in FPRs.  If there
	     is still space available in the register parameter area
	     *after* that amount, a part of the argument will be passed
	     in GPRs.  In that case, the total amount passed in any
	     registers is equal to the amount that would have been passed
	     in GPRs if everything were passed there, so we fall back to
	     the GPR code below to compute the appropriate value.  */
	  int fpr = ((FP_ARG_MAX_REG + 1 - cum->fregno)
		     * MIN (8, GET_MODE_SIZE (elt_mode)));
	  int fpr_words = fpr / (TARGET_32BIT ? 4 : 8);

	  if (align_words + fpr_words < GP_ARG_NUM_REG)
	    passed_in_gprs = true;
	  else
	    ret = fpr;
	}
    }

  if (passed_in_gprs
      && align_words < GP_ARG_NUM_REG
      && GP_ARG_NUM_REG < align_words + rs6000_arg_size (arg.mode, arg.type))
    ret = (GP_ARG_NUM_REG - align_words) * (TARGET_32BIT ? 4 : 8);

  if (ret != 0 && TARGET_DEBUG_ARG)
    fprintf (stderr, "rs6000_arg_partial_bytes: %d\n", ret);

  return ret;
}

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.

   Under V.4, aggregates and long double are passed by reference.

   As an extension to all 32-bit ABIs, AltiVec vectors are passed by
   reference unless the AltiVec vector extension ABI is in force.

   As an extension to all ABIs, variable sized types are passed by
   reference.  */

bool
rs6000_pass_by_reference (cumulative_args_t, const function_arg_info &arg)
{
  if (!arg.type)
    return 0;

  if (DEFAULT_ABI == ABI_V4 && TARGET_IEEEQUAD
      && FLOAT128_IEEE_P (TYPE_MODE (arg.type)))
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: V4 IEEE 128-bit\n");
      return 1;
    }

  if (DEFAULT_ABI == ABI_V4 && AGGREGATE_TYPE_P (arg.type))
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: V4 aggregate\n");
      return 1;
    }

  if (int_size_in_bytes (arg.type) < 0)
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: variable size\n");
      return 1;
    }

  /* Allow -maltivec -mabi=no-altivec without warning.  Altivec vector
     modes only exist for GCC vector types if -maltivec.  */
  if (TARGET_32BIT && !TARGET_ALTIVEC_ABI && ALTIVEC_VECTOR_MODE (arg.mode))
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: AltiVec\n");
      return 1;
    }

  /* Pass synthetic vectors in memory.  */
  if (TREE_CODE (arg.type) == VECTOR_TYPE
      && int_size_in_bytes (arg.type) > (TARGET_ALTIVEC_ABI ? 16 : 8))
    {
      static bool warned_for_pass_big_vectors = false;
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: synthetic vector\n");
      if (!warned_for_pass_big_vectors)
	{
	  warning (OPT_Wpsabi, "GCC vector passed by reference: "
		   "non-standard ABI extension with no compatibility "
		   "guarantee");
	  warned_for_pass_big_vectors = true;
	}
      return 1;
    }

  return 0;
}

/* Process parameter of type TYPE after ARGS_SO_FAR parameters were
   already processes.  Return true if the parameter must be passed
   (fully or partially) on the stack.  */

static bool
rs6000_parm_needs_stack (cumulative_args_t args_so_far, tree type)
{
  int unsignedp;
  rtx entry_parm;

  /* Catch errors.  */
  if (type == NULL || type == error_mark_node)
    return true;

  /* Handle types with no storage requirement.  */
  if (TYPE_MODE (type) == VOIDmode)
    return false;

  /* Handle complex types.  */
  if (TREE_CODE (type) == COMPLEX_TYPE)
    return (rs6000_parm_needs_stack (args_so_far, TREE_TYPE (type))
	    || rs6000_parm_needs_stack (args_so_far, TREE_TYPE (type)));

  /* Handle transparent aggregates.  */
  if ((TREE_CODE (type) == UNION_TYPE || TREE_CODE (type) == RECORD_TYPE)
      && TYPE_TRANSPARENT_AGGR (type))
    type = TREE_TYPE (first_field (type));

  /* See if this arg was passed by invisible reference.  */
  function_arg_info arg (type, /*named=*/true);
  apply_pass_by_reference_rules (get_cumulative_args (args_so_far), arg);

  /* Find mode as it is passed by the ABI.  */
  unsignedp = TYPE_UNSIGNED (type);
  arg.mode = promote_mode (arg.type, arg.mode, &unsignedp);

  /* If we must pass in stack, we need a stack.  */
  if (rs6000_must_pass_in_stack (arg))
    return true;

  /* If there is no incoming register, we need a stack.  */
  entry_parm = rs6000_function_arg (args_so_far, arg);
  if (entry_parm == NULL)
    return true;

  /* Likewise if we need to pass both in registers and on the stack.  */
  if (GET_CODE (entry_parm) == PARALLEL
      && XEXP (XVECEXP (entry_parm, 0, 0), 0) == NULL_RTX)
    return true;

  /* Also true if we're partially in registers and partially not.  */
  if (rs6000_arg_partial_bytes (args_so_far, arg) != 0)
    return true;

  /* Update info on where next arg arrives in registers.  */
  rs6000_function_arg_advance (args_so_far, arg);
  return false;
}

/* Return true if FUN has no prototype, has a variable argument
   list, or passes any parameter in memory.  */

static bool
rs6000_function_parms_need_stack (tree fun, bool incoming)
{
  tree fntype, result;
  CUMULATIVE_ARGS args_so_far_v;
  cumulative_args_t args_so_far;

  if (!fun)
    /* Must be a libcall, all of which only use reg parms.  */
    return false;

  fntype = fun;
  if (!TYPE_P (fun))
    fntype = TREE_TYPE (fun);

  /* Varargs functions need the parameter save area.  */
  if ((!incoming && !prototype_p (fntype)) || stdarg_p (fntype))
    return true;

  INIT_CUMULATIVE_INCOMING_ARGS (args_so_far_v, fntype, NULL_RTX);
  args_so_far = pack_cumulative_args (&args_so_far_v);

  /* When incoming, we will have been passed the function decl.
     It is necessary to use the decl to handle K&R style functions,
     where TYPE_ARG_TYPES may not be available.  */
  if (incoming)
    {
      gcc_assert (DECL_P (fun));
      result = DECL_RESULT (fun);
    }
  else
    result = TREE_TYPE (fntype);

  if (result && aggregate_value_p (result, fntype))
    {
      if (!TYPE_P (result))
	result = TREE_TYPE (result);
      result = build_pointer_type (result);
      rs6000_parm_needs_stack (args_so_far, result);
    }

  if (incoming)
    {
      tree parm;

      for (parm = DECL_ARGUMENTS (fun);
	   parm && parm != void_list_node;
	   parm = TREE_CHAIN (parm))
	if (rs6000_parm_needs_stack (args_so_far, TREE_TYPE (parm)))
	  return true;
    }
  else
    {
      function_args_iterator args_iter;
      tree arg_type;

      FOREACH_FUNCTION_ARGS (fntype, arg_type, args_iter)
	if (rs6000_parm_needs_stack (args_so_far, arg_type))
	  return true;
    }

  return false;
}

/* Return the size of the REG_PARM_STACK_SPACE are for FUN.  This is
   usually a constant depending on the ABI.  However, in the ELFv2 ABI
   the register parameter area is optional when calling a function that
   has a prototype is scope, has no variable argument list, and passes
   all parameters in registers.  */

int
rs6000_reg_parm_stack_space (tree fun, bool incoming)
{
  int reg_parm_stack_space;

  switch (DEFAULT_ABI)
    {
    default:
      reg_parm_stack_space = 0;
      break;

    case ABI_AIX:
    case ABI_DARWIN:
      reg_parm_stack_space = TARGET_64BIT ? 64 : 32;
      break;

    case ABI_ELFv2:
      /* ??? Recomputing this every time is a bit expensive.  Is there
	 a place to cache this information?  */
      if (rs6000_function_parms_need_stack (fun, incoming))
	reg_parm_stack_space = TARGET_64BIT ? 64 : 32;
      else
	reg_parm_stack_space = 0;
      break;
    }

  return reg_parm_stack_space;
}

static void
rs6000_move_block_from_reg (int regno, rtx x, int nregs)
{
  int i;
  machine_mode reg_mode = TARGET_32BIT ? SImode : DImode;

  if (nregs == 0)
    return;

  for (i = 0; i < nregs; i++)
    {
      rtx tem = adjust_address_nv (x, reg_mode, i * GET_MODE_SIZE (reg_mode));
      if (reload_completed)
	{
	  if (! strict_memory_address_p (reg_mode, XEXP (tem, 0)))
	    tem = NULL_RTX;
	  else
	    tem = simplify_gen_subreg (reg_mode, x, BLKmode,
				       i * GET_MODE_SIZE (reg_mode));
	}
      else
	tem = replace_equiv_address (tem, XEXP (tem, 0));

      gcc_assert (tem);

      emit_move_insn (tem, gen_rtx_REG (reg_mode, regno + i));
    }
}

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments.

   CUM is as above.

   ARG is the last named argument.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed.  */

void
setup_incoming_varargs (cumulative_args_t cum,
			const function_arg_info &arg,
			int *pretend_size ATTRIBUTE_UNUSED, int no_rtl)
{
  CUMULATIVE_ARGS next_cum;
  int reg_size = TARGET_32BIT ? 4 : 8;
  rtx save_area = NULL_RTX, mem;
  int first_reg_offset;
  alias_set_type set;

  /* Skip the last named argument.  */
  next_cum = *get_cumulative_args (cum);
  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl)))
    rs6000_function_arg_advance_1 (&next_cum, arg.mode, arg.type, arg.named,
				   0);

  if (DEFAULT_ABI == ABI_V4)
    {
      first_reg_offset = next_cum.sysv_gregno - GP_ARG_MIN_REG;

      if (! no_rtl)
	{
	  int gpr_reg_num = 0, gpr_size = 0, fpr_size = 0;
	  HOST_WIDE_INT offset = 0;

	  /* Try to optimize the size of the varargs save area.
	     The ABI requires that ap.reg_save_area is doubleword
	     aligned, but we don't need to allocate space for all
	     the bytes, only those to which we actually will save
	     anything.  */
	  if (cfun->va_list_gpr_size && first_reg_offset < GP_ARG_NUM_REG)
	    gpr_reg_num = GP_ARG_NUM_REG - first_reg_offset;
	  if (TARGET_HARD_FLOAT
	      && next_cum.fregno <= FP_ARG_V4_MAX_REG
	      && cfun->va_list_fpr_size)
	    {
	      if (gpr_reg_num)
		fpr_size = (next_cum.fregno - FP_ARG_MIN_REG)
			   * UNITS_PER_FP_WORD;
	      if (cfun->va_list_fpr_size
		  < FP_ARG_V4_MAX_REG + 1 - next_cum.fregno)
		fpr_size += cfun->va_list_fpr_size * UNITS_PER_FP_WORD;
	      else
		fpr_size += (FP_ARG_V4_MAX_REG + 1 - next_cum.fregno)
			    * UNITS_PER_FP_WORD;
	    }
	  if (gpr_reg_num)
	    {
	      offset = -((first_reg_offset * reg_size) & ~7);
	      if (!fpr_size && gpr_reg_num > cfun->va_list_gpr_size)
		{
		  gpr_reg_num = cfun->va_list_gpr_size;
		  if (reg_size == 4 && (first_reg_offset & 1))
		    gpr_reg_num++;
		}
	      gpr_size = (gpr_reg_num * reg_size + 7) & ~7;
	    }
	  else if (fpr_size)
	    offset = - (int) (next_cum.fregno - FP_ARG_MIN_REG)
		       * UNITS_PER_FP_WORD
		     - (int) (GP_ARG_NUM_REG * reg_size);

	  if (gpr_size + fpr_size)
	    {
	      rtx reg_save_area
		= assign_stack_local (BLKmode, gpr_size + fpr_size, 64);
	      gcc_assert (MEM_P (reg_save_area));
	      reg_save_area = XEXP (reg_save_area, 0);
	      if (GET_CODE (reg_save_area) == PLUS)
		{
		  gcc_assert (XEXP (reg_save_area, 0)
			      == virtual_stack_vars_rtx);
		  gcc_assert (CONST_INT_P (XEXP (reg_save_area, 1)));
		  offset += INTVAL (XEXP (reg_save_area, 1));
		}
	      else
		gcc_assert (reg_save_area == virtual_stack_vars_rtx);
	    }

	  cfun->machine->varargs_save_offset = offset;
	  save_area = plus_constant (Pmode, virtual_stack_vars_rtx, offset);
	}
    }
  else
    {
      first_reg_offset = next_cum.words;
      save_area = crtl->args.internal_arg_pointer;

      if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl))
	  && targetm.calls.must_pass_in_stack (arg))
	first_reg_offset += rs6000_arg_size (TYPE_MODE (arg.type), arg.type);
    }

  set = get_varargs_alias_set ();
  if (! no_rtl && first_reg_offset < GP_ARG_NUM_REG
      && cfun->va_list_gpr_size)
    {
      int n_gpr, nregs = GP_ARG_NUM_REG - first_reg_offset;

      if (va_list_gpr_counter_field)
	/* V4 va_list_gpr_size counts number of registers needed.  */
	n_gpr = cfun->va_list_gpr_size;
      else
	/* char * va_list instead counts number of bytes needed.  */
	n_gpr = (cfun->va_list_gpr_size + reg_size - 1) / reg_size;

      if (nregs > n_gpr)
	nregs = n_gpr;

      mem = gen_rtx_MEM (BLKmode,
			 plus_constant (Pmode, save_area,
					first_reg_offset * reg_size));
      MEM_NOTRAP_P (mem) = 1;
      set_mem_alias_set (mem, set);
      set_mem_align (mem, BITS_PER_WORD);

      rs6000_move_block_from_reg (GP_ARG_MIN_REG + first_reg_offset, mem,
				  nregs);
    }

  /* Save FP registers if needed.  */
  if (DEFAULT_ABI == ABI_V4
      && TARGET_HARD_FLOAT
      && ! no_rtl
      && next_cum.fregno <= FP_ARG_V4_MAX_REG
      && cfun->va_list_fpr_size)
    {
      int fregno = next_cum.fregno, nregs;
      rtx cr1 = gen_rtx_REG (CCmode, CR1_REGNO);
      rtx lab = gen_label_rtx ();
      int off = (GP_ARG_NUM_REG * reg_size) + ((fregno - FP_ARG_MIN_REG)
					       * UNITS_PER_FP_WORD);

      emit_jump_insn
	(gen_rtx_SET (pc_rtx,
		      gen_rtx_IF_THEN_ELSE (VOIDmode,
					    gen_rtx_NE (VOIDmode, cr1,
							const0_rtx),
					    gen_rtx_LABEL_REF (VOIDmode, lab),
					    pc_rtx)));

      for (nregs = 0;
	   fregno <= FP_ARG_V4_MAX_REG && nregs < cfun->va_list_fpr_size;
	   fregno++, off += UNITS_PER_FP_WORD, nregs++)
	{
	  mem = gen_rtx_MEM (TARGET_HARD_FLOAT ? DFmode : SFmode,
                             plus_constant (Pmode, save_area, off));
  	  MEM_NOTRAP_P (mem) = 1;
  	  set_mem_alias_set (mem, set);
	  set_mem_align (mem, GET_MODE_ALIGNMENT (
			 TARGET_HARD_FLOAT ? DFmode : SFmode));
	  emit_move_insn (mem, gen_rtx_REG (
                          TARGET_HARD_FLOAT ? DFmode : SFmode, fregno));
	}

      emit_label (lab);
    }
}

/* Create the va_list data type.  */

tree
rs6000_build_builtin_va_list (void)
{
  tree f_gpr, f_fpr, f_res, f_ovf, f_sav, record, type_decl;

  /* For AIX, prefer 'char *' because that's what the system
     header files like.  */
  if (DEFAULT_ABI != ABI_V4)
    return build_pointer_type (char_type_node);

  record = (*lang_hooks.types.make_type) (RECORD_TYPE);
  type_decl = build_decl (BUILTINS_LOCATION, TYPE_DECL,
      			  get_identifier ("__va_list_tag"), record);

  f_gpr = build_decl (BUILTINS_LOCATION, FIELD_DECL, get_identifier ("gpr"),
		      unsigned_char_type_node);
  f_fpr = build_decl (BUILTINS_LOCATION, FIELD_DECL, get_identifier ("fpr"),
		      unsigned_char_type_node);
  /* Give the two bytes of padding a name, so that -Wpadded won't warn on
     every user file.  */
  f_res = build_decl (BUILTINS_LOCATION, FIELD_DECL,
      		      get_identifier ("reserved"), short_unsigned_type_node);
  f_ovf = build_decl (BUILTINS_LOCATION, FIELD_DECL,
      		      get_identifier ("overflow_arg_area"),
		      ptr_type_node);
  f_sav = build_decl (BUILTINS_LOCATION, FIELD_DECL,
      		      get_identifier ("reg_save_area"),
		      ptr_type_node);

  va_list_gpr_counter_field = f_gpr;
  va_list_fpr_counter_field = f_fpr;

  DECL_FIELD_CONTEXT (f_gpr) = record;
  DECL_FIELD_CONTEXT (f_fpr) = record;
  DECL_FIELD_CONTEXT (f_res) = record;
  DECL_FIELD_CONTEXT (f_ovf) = record;
  DECL_FIELD_CONTEXT (f_sav) = record;

  TYPE_STUB_DECL (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_gpr;
  DECL_CHAIN (f_gpr) = f_fpr;
  DECL_CHAIN (f_fpr) = f_res;
  DECL_CHAIN (f_res) = f_ovf;
  DECL_CHAIN (f_ovf) = f_sav;

  layout_type (record);

  /* The correct type is an array type of one element.  */
  return build_array_type (record, build_index_type (size_zero_node));
}

/* Implement va_start.  */

void
rs6000_va_start (tree valist, rtx nextarg)
{
  HOST_WIDE_INT words, n_gpr, n_fpr;
  tree f_gpr, f_fpr, f_res, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;

  /* Only SVR4 needs something special.  */
  if (DEFAULT_ABI != ABI_V4)
    {
      std_expand_builtin_va_start (valist, nextarg);
      return;
    }

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = DECL_CHAIN (f_gpr);
  f_res = DECL_CHAIN (f_fpr);
  f_ovf = DECL_CHAIN (f_res);
  f_sav = DECL_CHAIN (f_ovf);

  valist = build_simple_mem_ref (valist);
  gpr = build3 (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr, NULL_TREE);
  fpr = build3 (COMPONENT_REF, TREE_TYPE (f_fpr), unshare_expr (valist),
		f_fpr, NULL_TREE);
  ovf = build3 (COMPONENT_REF, TREE_TYPE (f_ovf), unshare_expr (valist),
		f_ovf, NULL_TREE);
  sav = build3 (COMPONENT_REF, TREE_TYPE (f_sav), unshare_expr (valist),
		f_sav, NULL_TREE);

  /* Count number of gp and fp argument registers used.  */
  words = crtl->args.info.words;
  n_gpr = MIN (crtl->args.info.sysv_gregno - GP_ARG_MIN_REG,
	       GP_ARG_NUM_REG);
  n_fpr = MIN (crtl->args.info.fregno - FP_ARG_MIN_REG,
	       FP_ARG_NUM_REG);

  if (TARGET_DEBUG_ARG)
    fprintf (stderr, "va_start: words = " HOST_WIDE_INT_PRINT_DEC", n_gpr = "
	     HOST_WIDE_INT_PRINT_DEC", n_fpr = " HOST_WIDE_INT_PRINT_DEC"\n",
	     words, n_gpr, n_fpr);

  if (cfun->va_list_gpr_size)
    {
      t = build2 (MODIFY_EXPR, TREE_TYPE (gpr), gpr,
		  build_int_cst (NULL_TREE, n_gpr));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  if (cfun->va_list_fpr_size)
    {
      t = build2 (MODIFY_EXPR, TREE_TYPE (fpr), fpr,
		  build_int_cst (NULL_TREE, n_fpr));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

#ifdef HAVE_AS_GNU_ATTRIBUTE
      if (call_ABI_of_interest (cfun->decl))
	rs6000_passes_float = true;
#endif
    }

  /* Find the overflow area.  */
  t = make_tree (TREE_TYPE (ovf), crtl->args.internal_arg_pointer);
  if (words != 0)
    t = fold_build_pointer_plus_hwi (t, words * MIN_UNITS_PER_WORD);
  t = build2 (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* If there were no va_arg invocations, don't set up the register
     save area.  */
  if (!cfun->va_list_gpr_size
      && !cfun->va_list_fpr_size
      && n_gpr < GP_ARG_NUM_REG
      && n_fpr < FP_ARG_V4_MAX_REG)
    return;

  /* Find the register save area.  */
  t = make_tree (TREE_TYPE (sav), virtual_stack_vars_rtx);
  if (cfun->machine->varargs_save_offset)
    t = fold_build_pointer_plus_hwi (t, cfun->machine->varargs_save_offset);
  t = build2 (MODIFY_EXPR, TREE_TYPE (sav), sav, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement va_arg.  */

tree
rs6000_gimplify_va_arg (tree valist, tree type, gimple_seq *pre_p,
			gimple_seq *post_p)
{
  tree f_gpr, f_fpr, f_res, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, reg, t, u;
  int size, rsize, n_reg, sav_ofs, sav_scale;
  tree lab_false, lab_over, addr;
  int align;
  tree ptrtype = build_pointer_type_for_mode (type, ptr_mode, true);
  int regalign = 0;
  gimple *stmt;

  if (pass_va_arg_by_reference (type))
    {
      t = rs6000_gimplify_va_arg (valist, ptrtype, pre_p, post_p);
      return build_va_arg_indirect_ref (t);
    }

  /* We need to deal with the fact that the darwin ppc64 ABI is defined by an
     earlier version of gcc, with the property that it always applied alignment
     adjustments to the va-args (even for zero-sized types).  The cheapest way
     to deal with this is to replicate the effect of the part of 
     std_gimplify_va_arg_expr that carries out the align adjust, for the case 
     of relevance.  
     We don't need to check for pass-by-reference because of the test above.
     We can return a simplifed answer, since we know there's no offset to add.  */

  if (((TARGET_MACHO
        && rs6000_darwin64_abi)
       || DEFAULT_ABI == ABI_ELFv2
       || (DEFAULT_ABI == ABI_AIX && !rs6000_compat_align_parm))
      && integer_zerop (TYPE_SIZE (type)))
    {
      unsigned HOST_WIDE_INT align, boundary;
      tree valist_tmp = get_initialized_tmp_var (valist, pre_p, NULL);
      align = PARM_BOUNDARY / BITS_PER_UNIT;
      boundary = rs6000_function_arg_boundary (TYPE_MODE (type), type);
      if (boundary > MAX_SUPPORTED_STACK_ALIGNMENT)
	boundary = MAX_SUPPORTED_STACK_ALIGNMENT;
      boundary /= BITS_PER_UNIT;
      if (boundary > align)
	{
	  tree t ;
	  /* This updates arg ptr by the amount that would be necessary
	     to align the zero-sized (but not zero-alignment) item.  */
	  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		      fold_build_pointer_plus_hwi (valist_tmp, boundary - 1));
	  gimplify_and_add (t, pre_p);

	  t = fold_convert (sizetype, valist_tmp);
	  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		  fold_convert (TREE_TYPE (valist),
				fold_build2 (BIT_AND_EXPR, sizetype, t,
					     size_int (-boundary))));
	  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
	  gimplify_and_add (t, pre_p);
	}
      /* Since it is zero-sized there's no increment for the item itself. */
      valist_tmp = fold_convert (build_pointer_type (type), valist_tmp);
      return build_va_arg_indirect_ref (valist_tmp);
    }

  if (DEFAULT_ABI != ABI_V4)
    {
      if (targetm.calls.split_complex_arg && TREE_CODE (type) == COMPLEX_TYPE)
	{
	  tree elem_type = TREE_TYPE (type);
	  machine_mode elem_mode = TYPE_MODE (elem_type);
	  int elem_size = GET_MODE_SIZE (elem_mode);

	  if (elem_size < UNITS_PER_WORD)
	    {
	      tree real_part, imag_part;
	      gimple_seq post = NULL;

	      real_part = rs6000_gimplify_va_arg (valist, elem_type, pre_p,
						  &post);
	      /* Copy the value into a temporary, lest the formal temporary
		 be reused out from under us.  */
	      real_part = get_initialized_tmp_var (real_part, pre_p, &post);
	      gimple_seq_add_seq (pre_p, post);

	      imag_part = rs6000_gimplify_va_arg (valist, elem_type, pre_p,
						  post_p);

	      return build2 (COMPLEX_EXPR, type, real_part, imag_part);
	    }
	}

      return std_gimplify_va_arg_expr (valist, type, pre_p, post_p);
    }

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = DECL_CHAIN (f_gpr);
  f_res = DECL_CHAIN (f_fpr);
  f_ovf = DECL_CHAIN (f_res);
  f_sav = DECL_CHAIN (f_ovf);

  gpr = build3 (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr, NULL_TREE);
  fpr = build3 (COMPONENT_REF, TREE_TYPE (f_fpr), unshare_expr (valist),
		f_fpr, NULL_TREE);
  ovf = build3 (COMPONENT_REF, TREE_TYPE (f_ovf), unshare_expr (valist),
		f_ovf, NULL_TREE);
  sav = build3 (COMPONENT_REF, TREE_TYPE (f_sav), unshare_expr (valist),
		f_sav, NULL_TREE);

  size = int_size_in_bytes (type);
  rsize = (size + 3) / 4;
  int pad = 4 * rsize - size;
  align = 1;

  machine_mode mode = TYPE_MODE (type);
  if (abi_v4_pass_in_fpr (mode, false))
    {
      /* FP args go in FP registers, if present.  */
      reg = fpr;
      n_reg = (size + 7) / 8;
      sav_ofs = (TARGET_HARD_FLOAT ? 8 : 4) * 4;
      sav_scale = (TARGET_HARD_FLOAT ? 8 : 4);
      if (mode != SFmode && mode != SDmode)
	align = 8;
    }
  else
    {
      /* Otherwise into GP registers.  */
      reg = gpr;
      n_reg = rsize;
      sav_ofs = 0;
      sav_scale = 4;
      if (n_reg == 2)
	align = 8;
    }

  /* Pull the value out of the saved registers....  */

  lab_over = NULL;
  addr = create_tmp_var (ptr_type_node, "addr");

  /*  AltiVec vectors never go in registers when -mabi=altivec.  */
  if (TARGET_ALTIVEC_ABI && ALTIVEC_VECTOR_MODE (mode))
    align = 16;
  else
    {
      lab_false = create_artificial_label (input_location);
      lab_over = create_artificial_label (input_location);

      /* Long long is aligned in the registers.  As are any other 2 gpr
	 item such as complex int due to a historical mistake.  */
      u = reg;
      if (n_reg == 2 && reg == gpr)
	{
	  regalign = 1;
	  u = build2 (BIT_AND_EXPR, TREE_TYPE (reg), unshare_expr (reg),
		     build_int_cst (TREE_TYPE (reg), n_reg - 1));
	  u = build2 (POSTINCREMENT_EXPR, TREE_TYPE (reg),
		      unshare_expr (reg), u);
	}
      /* _Decimal128 is passed in even/odd fpr pairs; the stored
	 reg number is 0 for f1, so we want to make it odd.  */
      else if (reg == fpr && mode == TDmode)
	{
	  t = build2 (BIT_IOR_EXPR, TREE_TYPE (reg), unshare_expr (reg),
		      build_int_cst (TREE_TYPE (reg), 1));
	  u = build2 (MODIFY_EXPR, void_type_node, unshare_expr (reg), t);
	}

      t = fold_convert (TREE_TYPE (reg), size_int (8 - n_reg + 1));
      t = build2 (GE_EXPR, boolean_type_node, u, t);
      u = build1 (GOTO_EXPR, void_type_node, lab_false);
      t = build3 (COND_EXPR, void_type_node, t, u, NULL_TREE);
      gimplify_and_add (t, pre_p);

      t = sav;
      if (sav_ofs)
	t = fold_build_pointer_plus_hwi (sav, sav_ofs);

      u = build2 (POSTINCREMENT_EXPR, TREE_TYPE (reg), unshare_expr (reg),
		  build_int_cst (TREE_TYPE (reg), n_reg));
      u = fold_convert (sizetype, u);
      u = build2 (MULT_EXPR, sizetype, u, size_int (sav_scale));
      t = fold_build_pointer_plus (t, u);

      /* _Decimal32 varargs are located in the second word of the 64-bit
	 FP register for 32-bit binaries.  */
      if (TARGET_32BIT && TARGET_HARD_FLOAT && mode == SDmode)
	t = fold_build_pointer_plus_hwi (t, size);

      /* Args are passed right-aligned.  */
      if (BYTES_BIG_ENDIAN)
	t = fold_build_pointer_plus_hwi (t, pad);

      gimplify_assign (addr, t, pre_p);

      gimple_seq_add_stmt (pre_p, gimple_build_goto (lab_over));

      stmt = gimple_build_label (lab_false);
      gimple_seq_add_stmt (pre_p, stmt);

      if ((n_reg == 2 && !regalign) || n_reg > 2)
	{
	  /* Ensure that we don't find any more args in regs.
	     Alignment has taken care of for special cases.  */
	  gimplify_assign (reg, build_int_cst (TREE_TYPE (reg), 8), pre_p);
	}
    }

  /* ... otherwise out of the overflow area.  */

  /* Care for on-stack alignment if needed.  */
  t = ovf;
  if (align != 1)
    {
      t = fold_build_pointer_plus_hwi (t, align - 1);
      t = build2 (BIT_AND_EXPR, TREE_TYPE (t), t,
		  build_int_cst (TREE_TYPE (t), -align));
    }

  /* Args are passed right-aligned.  */
  if (BYTES_BIG_ENDIAN)
    t = fold_build_pointer_plus_hwi (t, pad);

  gimplify_expr (&t, pre_p, NULL, is_gimple_val, fb_rvalue);

  gimplify_assign (unshare_expr (addr), t, pre_p);

  t = fold_build_pointer_plus_hwi (t, size);
  gimplify_assign (unshare_expr (ovf), t, pre_p);

  if (lab_over)
    {
      stmt = gimple_build_label (lab_over);
      gimple_seq_add_stmt (pre_p, stmt);
    }

  if (STRICT_ALIGNMENT
      && (TYPE_ALIGN (type)
	  > (unsigned) BITS_PER_UNIT * (align < 4 ? 4 : align)))
    {
      /* The value (of type complex double, for example) may not be
	 aligned in memory in the saved registers, so copy via a
	 temporary.  (This is the same code as used for SPARC.)  */
      tree tmp = create_tmp_var (type, "va_arg_tmp");
      tree dest_addr = build_fold_addr_expr (tmp);

      tree copy = build_call_expr (builtin_decl_implicit (BUILT_IN_MEMCPY),
				   3, dest_addr, addr, size_int (rsize * 4));
      TREE_ADDRESSABLE (tmp) = 1;

      gimplify_and_add (copy, pre_p);
      addr = dest_addr;
    }

  addr = fold_convert (ptrtype, addr);
  return build_va_arg_indirect_ref (addr);
}

/* Return the permutation index for the swapping on the given vector mode.
   Note that the permutation index is correspondingly generated by endianness,
   it should be used by direct vector permutation.  */

rtx
swap_endian_selector_for_mode (machine_mode mode)
{
  unsigned int swap1[16] = {15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0};
  unsigned int swap2[16] = {7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};
  unsigned int swap4[16] = {3,2,1,0,7,6,5,4,11,10,9,8,15,14,13,12};
  unsigned int swap8[16] = {1,0,3,2,5,4,7,6,9,8,11,10,13,12,15,14};

  unsigned int *swaparray, i;
  rtx perm[16];

  switch (mode)
    {
    case E_V1TImode:
      swaparray = swap1;
      break;
    case E_V2DFmode:
    case E_V2DImode:
      swaparray = swap2;
      break;
    case E_V4SFmode:
    case E_V4SImode:
      swaparray = swap4;
      break;
    case E_V8HImode:
      swaparray = swap8;
      break;
    default:
      gcc_unreachable ();
    }

  for (i = 0; i < 16; ++i)
    if (BYTES_BIG_ENDIAN)
      perm[i] = GEN_INT (swaparray[i]);
    else
      /* Generates the reversed perm for little endian.  */
      perm[i] = GEN_INT (~swaparray[i] & 0x0000001f);

  return force_reg (V16QImode, gen_rtx_CONST_VECTOR (V16QImode,
						     gen_rtvec_v (16, perm)));
}

/* Return the internal arg pointer used for function incoming
   arguments.  When -fsplit-stack, the arg pointer is r12 so we need
   to copy it to a pseudo in order for it to be preserved over calls
   and suchlike.  We'd really like to use a pseudo here for the
   internal arg pointer but data-flow analysis is not prepared to
   accept pseudos as live at the beginning of a function.  */

rtx
rs6000_internal_arg_pointer (void)
{
  if (flag_split_stack
     && (lookup_attribute ("no_split_stack", DECL_ATTRIBUTES (cfun->decl))
         == NULL))

    {
      if (cfun->machine->split_stack_arg_pointer == NULL_RTX)
	{
	  rtx pat;

	  cfun->machine->split_stack_arg_pointer = gen_reg_rtx (Pmode);
	  REG_POINTER (cfun->machine->split_stack_arg_pointer) = 1;

	  /* Put the pseudo initialization right after the note at the
	     beginning of the function.  */
	  pat = gen_rtx_SET (cfun->machine->split_stack_arg_pointer,
			     gen_rtx_REG (Pmode, 12));
	  push_topmost_sequence ();
	  emit_insn_after (pat, get_insns ());
	  pop_topmost_sequence ();
	}
      rtx ret = plus_constant (Pmode, cfun->machine->split_stack_arg_pointer,
			       FIRST_PARM_OFFSET (current_function_decl));
      return copy_to_reg (ret);
    }
  return virtual_incoming_args_rtx;
}


/* A C compound statement that outputs the assembler code for a thunk
   function, used to implement C++ virtual function calls with
   multiple inheritance.  The thunk acts as a wrapper around a virtual
   function, adjusting the implicit object parameter before handing
   control off to the real function.

   First, emit code to add the integer DELTA to the location that
   contains the incoming first argument.  Assume that this argument
   contains a pointer, and is the one used to pass the `this' pointer
   in C++.  This is the incoming argument *before* the function
   prologue, e.g. `%o0' on a sparc.  The addition must preserve the
   values of all other incoming arguments.

   After the addition, emit code to jump to FUNCTION, which is a
   `FUNCTION_DECL'.  This is a direct pure jump, not a call, and does
   not touch the return address.  Hence returning from FUNCTION will
   return to whoever called the current `thunk'.

   The effect must be as if FUNCTION had been called directly with the
   adjusted first argument.  This macro is responsible for emitting
   all of the code for a thunk function; output_function_prologue()
   and output_function_epilogue() are not invoked.

   The THUNK_FNDECL is redundant.  (DELTA and FUNCTION have already
   been extracted from it.)  It might possibly be useful on some
   targets, but probably not.

   If you do not define this macro, the target-independent code in the
   C++ frontend will generate a less efficient heavyweight thunk that
   calls FUNCTION instead of jumping to it.  The generic approach does
   not support varargs.  */

void
rs6000_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
			HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
			tree function)
{
  const char *fnname = get_fnname_from_decl (thunk_fndecl);
  rtx this_rtx, funexp;
  rtx_insn *insn;

  reload_completed = 1;
  epilogue_completed = 1;

  /* Mark the end of the (empty) prologue.  */
  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Find the "this" pointer.  If the function returns a structure,
     the structure return pointer is in r3.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, 4);
  else
    this_rtx = gen_rtx_REG (Pmode, 3);

  /* Apply the constant offset, if required.  */
  if (delta)
    emit_insn (gen_add3_insn (this_rtx, this_rtx, GEN_INT (delta)));

  /* Apply the offset from the vtable, if required.  */
  if (vcall_offset)
    {
      rtx vcall_offset_rtx = GEN_INT (vcall_offset);
      rtx tmp = gen_rtx_REG (Pmode, 12);

      emit_move_insn (tmp, gen_rtx_MEM (Pmode, this_rtx));
      if (((unsigned HOST_WIDE_INT) vcall_offset) + 0x8000 >= 0x10000)
	{
	  emit_insn (gen_add3_insn (tmp, tmp, vcall_offset_rtx));
	  emit_move_insn (tmp, gen_rtx_MEM (Pmode, tmp));
	}
      else
	{
	  rtx loc = gen_rtx_PLUS (Pmode, tmp, vcall_offset_rtx);

	  emit_move_insn (tmp, gen_rtx_MEM (Pmode, loc));
	}
      emit_insn (gen_add3_insn (this_rtx, this_rtx, tmp));
    }

  /* Generate a tail call to the target function.  */
  if (!TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);
  funexp = gen_rtx_MEM (FUNCTION_MODE, funexp);

  insn = emit_call_insn (gen_sibcall (funexp, const0_rtx, const0_rtx));
  SIBLING_CALL_P (insn) = 1;
  emit_barrier ();

  /* Run just enough of rest_of_compilation to get the insns emitted.
     There's not really enough bulk here to make other passes such as
     instruction scheduling worth while.  */
  insn = get_insns ();
  shorten_branches (insn);
  assemble_start_function (thunk_fndecl, fnname);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();
  assemble_end_function (thunk_fndecl, fnname);

  reload_completed = 0;
  epilogue_completed = 0;
}
