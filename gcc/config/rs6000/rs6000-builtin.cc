/* Target-specific built-in function support for the Power architecture.
   See also rs6000-c.c, rs6000-gen-builtins.c, rs6000-builtins.def, and
   rs6000-overloads.def.
   Note that "normal" builtins (generic math functions, etc.) are handled
   in rs6000.c.

   Copyright (C) 2002-2025 Free Software Foundation, Inc.

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
#include "target.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "tm_p.h"
#include "optabs.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "ssa.h"
#include "tree-ssa-propagate.h"
#include "builtins.h"
#include "tree-vector-builder.h"
#include "ppc-auxv.h"
#include "rs6000-internal.h"

/* Built in types.  */
tree rs6000_builtin_types[RS6000_BTI_MAX];

/* Support targetm.vectorize.builtin_mask_for_load.  */
tree altivec_builtin_mask_for_load;

/* **** General support functions **** */

/* Raise an error message for a builtin function that is called without the
   appropriate target options being set.  */

void
rs6000_invalid_builtin (enum rs6000_gen_builtins fncode)
{
  size_t j = (size_t) fncode;
  const char *name = rs6000_builtin_info[j].bifname;

  switch (rs6000_builtin_info[j].enable)
    {
    case ENB_P5:
      error ("%qs requires the %qs option", name, "-mcpu=power5");
      break;
    case ENB_P6:
      error ("%qs requires the %qs option", name, "-mcpu=power6");
      break;
    case ENB_P6_64:
      error ("%qs requires the %qs option and either the %qs or %qs option",
	     name, "-mcpu=power6", "-m64", "-mpowerpc64");
      break;
    case ENB_ALTIVEC:
      error ("%qs requires the %qs option", name, "-maltivec");
      break;
    case ENB_CELL:
      error ("%qs requires the %qs option", name, "-mcpu=cell");
      break;
    case ENB_VSX:
      error ("%qs requires the %qs option", name, "-mvsx");
      break;
    case ENB_P7:
      error ("%qs requires the %qs option", name, "-mcpu=power7");
      break;
    case ENB_P7_64:
      error ("%qs requires the %qs option and either the %qs or %qs option",
	     name, "-mcpu=power7", "-m64", "-mpowerpc64");
      break;
    case ENB_P8:
      error ("%qs requires the %qs option", name, "-mcpu=power8");
      break;
    case ENB_P8V:
      error ("%qs requires the %qs and %qs options", name, "-mcpu=power8",
	     "-mvsx");
      break;
    case ENB_P9:
      error ("%qs requires the %qs option", name, "-mcpu=power9");
      break;
    case ENB_P9_64:
      error ("%qs requires the %qs option and either the %qs or %qs option",
	     name, "-mcpu=power9", "-m64", "-mpowerpc64");
      break;
    case ENB_P9V:
      error ("%qs requires the %qs and %qs options", name, "-mcpu=power9",
	     "-mvsx");
      break;
    case ENB_IEEE128_HW:
      error ("%qs requires quad-precision floating-point arithmetic", name);
      break;
    case ENB_DFP:
      error ("%qs requires the %qs option", name, "-mhard-dfp");
      break;
    case ENB_CRYPTO:
      error ("%qs requires the %qs option", name, "-mcrypto");
      break;
    case ENB_HTM:
      error ("%qs requires the %qs option", name, "-mhtm");
      break;
    case ENB_P10:
      error ("%qs requires the %qs option", name, "-mcpu=power10");
      break;
    case ENB_P10_64:
      error ("%qs requires the %qs option and either the %qs or %qs option",
	     name, "-mcpu=power10", "-m64", "-mpowerpc64");
      break;
    case ENB_MMA:
      error ("%qs requires the %qs option", name, "-mmma");
      break;
    default:
    case ENB_ALWAYS:
      gcc_unreachable ();
    }
}

/* Check whether a builtin function is supported in this target
   configuration.  */
bool
rs6000_builtin_is_supported (enum rs6000_gen_builtins fncode)
{
  switch (rs6000_builtin_info[(size_t) fncode].enable)
    {
    case ENB_ALWAYS:
      return true;
    case ENB_P5:
      return TARGET_POPCNTB;
    case ENB_P6:
      return TARGET_CMPB;
    case ENB_P6_64:
      return TARGET_CMPB && TARGET_POWERPC64;
    case ENB_P7:
      return TARGET_POPCNTD;
    case ENB_P7_64:
      return TARGET_POPCNTD && TARGET_POWERPC64;
    case ENB_P8:
      return TARGET_POWER8;
    case ENB_P8V:
      return TARGET_P8_VECTOR;
    case ENB_P9:
      return TARGET_MODULO;
    case ENB_P9_64:
      return TARGET_MODULO && TARGET_POWERPC64;
    case ENB_P9V:
      return TARGET_P9_VECTOR;
    case ENB_P10:
      return TARGET_POWER10;
    case ENB_P10_64:
      return TARGET_POWER10 && TARGET_POWERPC64;
    case ENB_ALTIVEC:
      return TARGET_ALTIVEC;
    case ENB_VSX:
      return TARGET_VSX;
    case ENB_CELL:
      return TARGET_ALTIVEC && rs6000_cpu == PROCESSOR_CELL;
    case ENB_IEEE128_HW:
      return TARGET_FLOAT128_HW;
    case ENB_DFP:
      return TARGET_DFP;
    case ENB_CRYPTO:
      return TARGET_CRYPTO;
    case ENB_HTM:
      return TARGET_HTM;
    case ENB_MMA:
      return TARGET_MMA;
    default:
      gcc_unreachable ();
    }
  gcc_unreachable ();
}

/* Target hook for early folding of built-ins, shamelessly stolen
   from ia64.cc.  */

tree
rs6000_fold_builtin (tree fndecl ATTRIBUTE_UNUSED,
		     int n_args ATTRIBUTE_UNUSED,
		     tree *args ATTRIBUTE_UNUSED,
		     bool ignore ATTRIBUTE_UNUSED)
{
#ifdef SUBTARGET_FOLD_BUILTIN
  return SUBTARGET_FOLD_BUILTIN (fndecl, n_args, args, ignore);
#else
  return NULL_TREE;
#endif
}

tree
rs6000_builtin_decl (unsigned code, bool /* initialize_p */)
{
  rs6000_gen_builtins fcode = (rs6000_gen_builtins) code;

  if (fcode >= RS6000_OVLD_MAX)
    return error_mark_node;

  return rs6000_builtin_decls[code];
}

/* Implement targetm.vectorize.builtin_mask_for_load.  */
tree
rs6000_builtin_mask_for_load (void)
{
  /* Don't use lvsl/vperm for P8 and similarly efficient machines.  */
  if ((TARGET_ALTIVEC && !TARGET_VSX)
      || (TARGET_VSX && !TARGET_EFFICIENT_UNALIGNED_VSX))
    return altivec_builtin_mask_for_load;
  else
    return 0;
}

/* Implement targetm.vectorize.builtin_md_vectorized_function.  */

tree
rs6000_builtin_md_vectorized_function (tree fndecl, tree type_out,
				       tree type_in)
{
  machine_mode in_mode, out_mode;
  int in_n, out_n;

  if (TARGET_DEBUG_BUILTIN)
    fprintf (stderr,
	     "rs6000_builtin_md_vectorized_function (%s, %s, %s)\n",
	     IDENTIFIER_POINTER (DECL_NAME (fndecl)),
	     GET_MODE_NAME (TYPE_MODE (type_out)),
	     GET_MODE_NAME (TYPE_MODE (type_in)));

  /* TODO: Should this be gcc_assert?  */
  if (TREE_CODE (type_out) != VECTOR_TYPE
      || TREE_CODE (type_in) != VECTOR_TYPE)
    return NULL_TREE;

  out_mode = TYPE_MODE (TREE_TYPE (type_out));
  out_n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);

  enum rs6000_gen_builtins fn
    = (enum rs6000_gen_builtins) DECL_MD_FUNCTION_CODE (fndecl);
  switch (fn)
    {
    case RS6000_BIF_RSQRTF:
      if (VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[RS6000_BIF_VRSQRTFP];
      break;
    case RS6000_BIF_RSQRT:
      if (VECTOR_UNIT_VSX_P (V2DFmode)
	  && out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return rs6000_builtin_decls[RS6000_BIF_RSQRT_2DF];
      break;
    case RS6000_BIF_RECIPF:
      if (VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[RS6000_BIF_VRECIPFP];
      break;
    case RS6000_BIF_RECIP:
      if (VECTOR_UNIT_VSX_P (V2DFmode)
	  && out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return rs6000_builtin_decls[RS6000_BIF_RECIP_V2DF];
      break;
    default:
      break;
    }

  machine_mode in_vmode = TYPE_MODE (type_in);
  machine_mode out_vmode = TYPE_MODE (type_out);

  /* Power10 supported vectorized built-in functions.  */
  if (TARGET_POWER10
      && in_vmode == out_vmode
      && VECTOR_UNIT_ALTIVEC_OR_VSX_P (in_vmode))
    {
      machine_mode exp_mode = DImode;
      machine_mode exp_vmode = V2DImode;
      enum rs6000_gen_builtins bif;
      switch (fn)
	{
	case RS6000_BIF_DIVWE:
	case RS6000_BIF_DIVWEU:
	  exp_mode = SImode;
	  exp_vmode = V4SImode;
	  if (fn == RS6000_BIF_DIVWE)
	    bif = RS6000_BIF_VDIVESW;
	  else
	    bif = RS6000_BIF_VDIVEUW;
	  break;
	case RS6000_BIF_DIVDE:
	case RS6000_BIF_DIVDEU:
	  if (fn == RS6000_BIF_DIVDE)
	    bif = RS6000_BIF_VDIVESD;
	  else
	    bif = RS6000_BIF_VDIVEUD;
	  break;
	case RS6000_BIF_CFUGED:
	  bif = RS6000_BIF_VCFUGED;
	  break;
	case RS6000_BIF_CNTLZDM:
	  bif = RS6000_BIF_VCLZDM;
	  break;
	case RS6000_BIF_CNTTZDM:
	  bif = RS6000_BIF_VCTZDM;
	  break;
	case RS6000_BIF_PDEPD:
	  bif = RS6000_BIF_VPDEPD;
	  break;
	case RS6000_BIF_PEXTD:
	  bif = RS6000_BIF_VPEXTD;
	  break;
	default:
	  return NULL_TREE;
	}

      if (in_mode == exp_mode && in_vmode == exp_vmode)
	return rs6000_builtin_decls[bif];
    }

  return NULL_TREE;
}

/* Returns a code for a target-specific builtin that implements
   reciprocal of the function, or NULL_TREE if not available.  */

tree
rs6000_builtin_reciprocal (tree fndecl)
{
  switch (DECL_MD_FUNCTION_CODE (fndecl))
    {
    case RS6000_BIF_XVSQRTDP:
      if (!RS6000_RECIP_AUTO_RSQRTE_P (V2DFmode))
	return NULL_TREE;

      return rs6000_builtin_decls[RS6000_BIF_RSQRT_2DF];

    case RS6000_BIF_XVSQRTSP:
      if (!RS6000_RECIP_AUTO_RSQRTE_P (V4SFmode))
	return NULL_TREE;

      return rs6000_builtin_decls[RS6000_BIF_RSQRT_4SF];

    default:
      return NULL_TREE;
    }
}

/* **** Initialization support **** */

/* Create a builtin vector type with a name.  Taking care not to give
   the canonical type a name.  */

static tree
rs6000_vector_type (const char *name, tree elt_type, unsigned num_elts)
{
  tree result = build_vector_type (elt_type, num_elts);

  /* Copy so we don't give the canonical type a name.  */
  result = build_variant_type_copy (result);

  add_builtin_type (name, result);

  return result;
}

/* Debug utility to translate a type node to a single textual token.  */
static
const char *rs6000_type_string (tree type_node)
{
  if (type_node == NULL_TREE)
    return "**NULL**";
  else if (type_node == void_type_node)
    return "void";
  else if (type_node == long_integer_type_node)
    return "long";
  else if (type_node == long_unsigned_type_node)
    return "ulong";
  else if (type_node == long_long_integer_type_node)
    return "longlong";
  else if (type_node == long_long_unsigned_type_node)
    return "ulonglong";
  else if (type_node == bool_V2DI_type_node)
    return "vbll";
  else if (type_node == bool_V4SI_type_node)
    return "vbi";
  else if (type_node == bool_V8HI_type_node)
    return "vbs";
  else if (type_node == bool_V16QI_type_node)
    return "vbc";
  else if (type_node == bool_int_type_node)
    return "bool";
  else if (type_node == dfloat64_type_node)
    return "_Decimal64";
  else if (type_node == double_type_node)
    return "double";
  else if (type_node == intDI_type_node)
    return "sll";
  else if (type_node == intHI_type_node)
    return "ss";
  else if (type_node == ibm128_float_type_node)
    return "__ibm128";
  else if (type_node == ieee128_float_type_node)
    return "__ieee128";
  else if (type_node == opaque_V4SI_type_node)
    return "opaque";
  else if (POINTER_TYPE_P (type_node))
    return "void*";
  else if (type_node == intQI_type_node || type_node == char_type_node)
    return "sc";
  else if (type_node == dfloat32_type_node)
    return "_Decimal32";
  else if (type_node == float_type_node)
    return "float";
  else if (type_node == intSI_type_node || type_node == integer_type_node)
    return "si";
  else if (type_node == dfloat128_type_node)
    return "_Decimal128";
  else if (type_node == long_double_type_node)
    return "longdouble";
  else if (type_node == intTI_type_node)
    return "sq";
  else if (type_node == unsigned_intDI_type_node)
    return "ull";
  else if (type_node == unsigned_intHI_type_node)
    return "us";
  else if (type_node == unsigned_intQI_type_node)
    return "uc";
  else if (type_node == unsigned_intSI_type_node)
    return "ui";
  else if (type_node == unsigned_intTI_type_node)
    return "uq";
  else if (type_node == unsigned_V1TI_type_node)
    return "vuq";
  else if (type_node == unsigned_V2DI_type_node)
    return "vull";
  else if (type_node == unsigned_V4SI_type_node)
    return "vui";
  else if (type_node == unsigned_V8HI_type_node)
    return "vus";
  else if (type_node == unsigned_V16QI_type_node)
    return "vuc";
  else if (type_node == V16QI_type_node)
    return "vsc";
  else if (type_node == V1TI_type_node)
    return "vsq";
  else if (type_node == V2DF_type_node)
    return "vd";
  else if (type_node == V2DI_type_node)
    return "vsll";
  else if (type_node == V4SF_type_node)
    return "vf";
  else if (type_node == V4SI_type_node)
    return "vsi";
  else if (type_node == V8HI_type_node)
    return "vss";
  else if (type_node == pixel_V8HI_type_node)
    return "vp";
  else if (type_node == pcvoid_type_node)
    return "voidc*";
  else if (type_node == float128_type_node)
    return "_Float128";
  else if (type_node == vector_pair_type_node)
    return "__vector_pair";
  else if (type_node == vector_quad_type_node)
    return "__vector_quad";

  return "unknown";
}

void
rs6000_init_builtins (void)
{
  tree tdecl;
  tree t;

  if (TARGET_DEBUG_BUILTIN)
    fprintf (stderr, "rs6000_init_builtins%s%s\n",
	     (TARGET_ALTIVEC)	   ? ", altivec" : "",
	     (TARGET_VSX)	   ? ", vsx"	 : "");

  V2DI_type_node = rs6000_vector_type ("__vector long long",
				       long_long_integer_type_node, 2);
  ptr_V2DI_type_node
    = build_pointer_type (build_qualified_type (V2DI_type_node,
						TYPE_QUAL_CONST));

  V2DF_type_node = rs6000_vector_type ("__vector double", double_type_node, 2);
  ptr_V2DF_type_node
    = build_pointer_type (build_qualified_type (V2DF_type_node,
						TYPE_QUAL_CONST));

  V4SI_type_node = rs6000_vector_type ("__vector signed int",
				       intSI_type_node, 4);
  ptr_V4SI_type_node
    = build_pointer_type (build_qualified_type (V4SI_type_node,
						TYPE_QUAL_CONST));

  V4SF_type_node = rs6000_vector_type ("__vector float", float_type_node, 4);
  ptr_V4SF_type_node
    = build_pointer_type (build_qualified_type (V4SF_type_node,
						TYPE_QUAL_CONST));

  V8HI_type_node = rs6000_vector_type ("__vector signed short",
				       intHI_type_node, 8);
  ptr_V8HI_type_node
    = build_pointer_type (build_qualified_type (V8HI_type_node,
						TYPE_QUAL_CONST));

  V16QI_type_node = rs6000_vector_type ("__vector signed char",
					intQI_type_node, 16);
  ptr_V16QI_type_node
    = build_pointer_type (build_qualified_type (V16QI_type_node,
						TYPE_QUAL_CONST));

  unsigned_V16QI_type_node = rs6000_vector_type ("__vector unsigned char",
					unsigned_intQI_type_node, 16);
  ptr_unsigned_V16QI_type_node
    = build_pointer_type (build_qualified_type (unsigned_V16QI_type_node,
						TYPE_QUAL_CONST));

  unsigned_V8HI_type_node = rs6000_vector_type ("__vector unsigned short",
				       unsigned_intHI_type_node, 8);
  ptr_unsigned_V8HI_type_node
    = build_pointer_type (build_qualified_type (unsigned_V8HI_type_node,
						TYPE_QUAL_CONST));

  unsigned_V4SI_type_node = rs6000_vector_type ("__vector unsigned int",
				       unsigned_intSI_type_node, 4);
  ptr_unsigned_V4SI_type_node
    = build_pointer_type (build_qualified_type (unsigned_V4SI_type_node,
						TYPE_QUAL_CONST));

  unsigned_V2DI_type_node
    = rs6000_vector_type ("__vector unsigned long long",
			  long_long_unsigned_type_node, 2);

  ptr_unsigned_V2DI_type_node
    = build_pointer_type (build_qualified_type (unsigned_V2DI_type_node,
						TYPE_QUAL_CONST));

  opaque_V4SI_type_node = build_opaque_vector_type (intSI_type_node, 4);

  const_str_type_node
    = build_pointer_type (build_qualified_type (char_type_node,
						TYPE_QUAL_CONST));

  /* We use V1TI mode as a special container to hold __int128_t items that
     must live in VSX registers.  */
  if (intTI_type_node)
    {
      V1TI_type_node = rs6000_vector_type ("__vector __int128",
					   intTI_type_node, 1);
      ptr_V1TI_type_node
	= build_pointer_type (build_qualified_type (V1TI_type_node,
						    TYPE_QUAL_CONST));
      unsigned_V1TI_type_node
	= rs6000_vector_type ("__vector unsigned __int128",
			      unsigned_intTI_type_node, 1);
      ptr_unsigned_V1TI_type_node
	= build_pointer_type (build_qualified_type (unsigned_V1TI_type_node,
						    TYPE_QUAL_CONST));
    }

  /* The 'vector bool ...' types must be kept distinct from 'vector unsigned ...'
     types, especially in C++ land.  Similarly, 'vector pixel' is distinct from
     'vector unsigned short'.  */

  bool_char_type_node = build_distinct_type_copy (unsigned_intQI_type_node);
  bool_short_type_node = build_distinct_type_copy (unsigned_intHI_type_node);
  bool_int_type_node = build_distinct_type_copy (unsigned_intSI_type_node);
  bool_long_long_type_node = build_distinct_type_copy (unsigned_intDI_type_node);
  pixel_type_node = build_distinct_type_copy (unsigned_intHI_type_node);

  long_integer_type_internal_node = long_integer_type_node;
  long_unsigned_type_internal_node = long_unsigned_type_node;
  long_long_integer_type_internal_node = long_long_integer_type_node;
  long_long_unsigned_type_internal_node = long_long_unsigned_type_node;
  intQI_type_internal_node = intQI_type_node;
  uintQI_type_internal_node = unsigned_intQI_type_node;
  intHI_type_internal_node = intHI_type_node;
  uintHI_type_internal_node = unsigned_intHI_type_node;
  intSI_type_internal_node = intSI_type_node;
  uintSI_type_internal_node = unsigned_intSI_type_node;
  intDI_type_internal_node = intDI_type_node;
  uintDI_type_internal_node = unsigned_intDI_type_node;
  intTI_type_internal_node = intTI_type_node;
  uintTI_type_internal_node = unsigned_intTI_type_node;
  float_type_internal_node = float_type_node;
  double_type_internal_node = double_type_node;
  long_double_type_internal_node = long_double_type_node;
  dfloat64_type_internal_node = dfloat64_type_node;
  dfloat128_type_internal_node = dfloat128_type_node;
  void_type_internal_node = void_type_node;

  ptr_intQI_type_node
    = build_pointer_type (build_qualified_type (intQI_type_internal_node,
						TYPE_QUAL_CONST));
  ptr_uintQI_type_node
    = build_pointer_type (build_qualified_type (uintQI_type_internal_node,
						TYPE_QUAL_CONST));
  ptr_intHI_type_node
    = build_pointer_type (build_qualified_type (intHI_type_internal_node,
						TYPE_QUAL_CONST));
  ptr_uintHI_type_node
    = build_pointer_type (build_qualified_type (uintHI_type_internal_node,
						TYPE_QUAL_CONST));
  ptr_intSI_type_node
    = build_pointer_type (build_qualified_type (intSI_type_internal_node,
						TYPE_QUAL_CONST));
  ptr_uintSI_type_node
    = build_pointer_type (build_qualified_type (uintSI_type_internal_node,
						TYPE_QUAL_CONST));
  ptr_intDI_type_node
    = build_pointer_type (build_qualified_type (intDI_type_internal_node,
						TYPE_QUAL_CONST));
  ptr_uintDI_type_node
    = build_pointer_type (build_qualified_type (uintDI_type_internal_node,
						TYPE_QUAL_CONST));
  ptr_intTI_type_node
    = build_pointer_type (build_qualified_type (intTI_type_internal_node,
						TYPE_QUAL_CONST));
  ptr_uintTI_type_node
    = build_pointer_type (build_qualified_type (uintTI_type_internal_node,
						TYPE_QUAL_CONST));

  t = build_qualified_type (long_integer_type_internal_node, TYPE_QUAL_CONST);
  ptr_long_integer_type_node = build_pointer_type (t);

  t = build_qualified_type (long_unsigned_type_internal_node, TYPE_QUAL_CONST);
  ptr_long_unsigned_type_node = build_pointer_type (t);

  ptr_float_type_node
    = build_pointer_type (build_qualified_type (float_type_internal_node,
						TYPE_QUAL_CONST));
  ptr_double_type_node
    = build_pointer_type (build_qualified_type (double_type_internal_node,
						TYPE_QUAL_CONST));
  ptr_long_double_type_node
    = build_pointer_type (build_qualified_type (long_double_type_internal_node,
						TYPE_QUAL_CONST));
  if (dfloat64_type_node)
    {
      t = build_qualified_type (dfloat64_type_internal_node, TYPE_QUAL_CONST);
      ptr_dfloat64_type_node = build_pointer_type (t);
    }
  else
    ptr_dfloat64_type_node = NULL;

  if (dfloat128_type_node)
    {
      t = build_qualified_type (dfloat128_type_internal_node, TYPE_QUAL_CONST);
      ptr_dfloat128_type_node = build_pointer_type (t);
    }
  else
    ptr_dfloat128_type_node = NULL;

  t = build_qualified_type (long_long_integer_type_internal_node,
			    TYPE_QUAL_CONST);
  ptr_long_long_integer_type_node  = build_pointer_type (t);

  t = build_qualified_type (long_long_unsigned_type_internal_node,
			    TYPE_QUAL_CONST);
  ptr_long_long_unsigned_type_node = build_pointer_type (t);

  /* 128-bit floating point support.  KFmode is IEEE 128-bit floating point.
     IFmode is the IBM extended 128-bit format that is a pair of doubles.
     TFmode will be either IEEE 128-bit floating point or the IBM double-double
     format that uses a pair of doubles, depending on the switches and
     defaults.

     If we don't support for either 128-bit IBM double double or IEEE 128-bit
     floating point, we need make sure the type is non-zero or else self-test
     fails during bootstrap.

     Always create __ibm128 as a separate type, even if the current long double
     format is IBM extended double.

     For IEEE 128-bit floating point, always create the type __ieee128.  If the
     user used -mfloat128, rs6000-c.cc will create a define from __float128 to
     __ieee128.  */
  if (TARGET_LONG_DOUBLE_128 && (!TARGET_IEEEQUAD || TARGET_FLOAT128_TYPE))
    {
      if (!TARGET_IEEEQUAD)
	ibm128_float_type_node = long_double_type_node;
      else
	{
	  ibm128_float_type_node = make_node (REAL_TYPE);
	  TYPE_PRECISION (ibm128_float_type_node) = 128;
	  SET_TYPE_MODE (ibm128_float_type_node, IFmode);
	  layout_type (ibm128_float_type_node);
	}
      t = build_qualified_type (ibm128_float_type_node, TYPE_QUAL_CONST);
      lang_hooks.types.register_builtin_type (ibm128_float_type_node,
					      "__ibm128");
    }
  else
    ibm128_float_type_node = NULL_TREE;

  if (TARGET_FLOAT128_TYPE)
    {
      if (TARGET_IEEEQUAD && TARGET_LONG_DOUBLE_128)
	ieee128_float_type_node = long_double_type_node;
      else
	{
	  /* For C we only need to register the __ieee128 name for
	     it.  For C++, we create a distinct type which will mangle
	     differently (u9__ieee128) vs. _Float128 (DF128_) and behave
	     backwards compatibly.  */
	  if (float128t_type_node == NULL_TREE)
	    {
	      float128t_type_node = make_node (REAL_TYPE);
	      TYPE_PRECISION (float128t_type_node)
		= TYPE_PRECISION (float128_type_node);
	      layout_type (float128t_type_node);
	      SET_TYPE_MODE (float128t_type_node,
			     TYPE_MODE (float128_type_node));
	    }
	  ieee128_float_type_node = float128t_type_node;
	}
      t = build_qualified_type (ieee128_float_type_node, TYPE_QUAL_CONST);
      lang_hooks.types.register_builtin_type (ieee128_float_type_node,
					      "__ieee128");
    }
  else
    ieee128_float_type_node = NULL_TREE;

  /* Vector pair and vector quad support.  */
  vector_pair_type_node = make_node (OPAQUE_TYPE);
  SET_TYPE_MODE (vector_pair_type_node, OOmode);
  TYPE_SIZE (vector_pair_type_node) = bitsize_int (GET_MODE_BITSIZE (OOmode));
  TYPE_PRECISION (vector_pair_type_node) = GET_MODE_BITSIZE (OOmode);
  TYPE_SIZE_UNIT (vector_pair_type_node) = size_int (GET_MODE_SIZE (OOmode));
  SET_TYPE_ALIGN (vector_pair_type_node, 256);
  TYPE_USER_ALIGN (vector_pair_type_node) = 0;
  lang_hooks.types.register_builtin_type (vector_pair_type_node,
					  "__vector_pair");
  t = build_qualified_type (vector_pair_type_node, TYPE_QUAL_CONST);
  ptr_vector_pair_type_node = build_pointer_type (t);

  vector_quad_type_node = make_node (OPAQUE_TYPE);
  SET_TYPE_MODE (vector_quad_type_node, XOmode);
  TYPE_SIZE (vector_quad_type_node) = bitsize_int (GET_MODE_BITSIZE (XOmode));
  TYPE_PRECISION (vector_quad_type_node) = GET_MODE_BITSIZE (XOmode);
  TYPE_SIZE_UNIT (vector_quad_type_node) = size_int (GET_MODE_SIZE (XOmode));
  SET_TYPE_ALIGN (vector_quad_type_node, 512);
  TYPE_USER_ALIGN (vector_quad_type_node) = 0;
  lang_hooks.types.register_builtin_type (vector_quad_type_node,
					  "__vector_quad");
  t = build_qualified_type (vector_quad_type_node, TYPE_QUAL_CONST);
  ptr_vector_quad_type_node = build_pointer_type (t);

  tdecl = add_builtin_type ("__bool char", bool_char_type_node);
  TYPE_NAME (bool_char_type_node) = tdecl;

  tdecl = add_builtin_type ("__bool short", bool_short_type_node);
  TYPE_NAME (bool_short_type_node) = tdecl;

  tdecl = add_builtin_type ("__bool int", bool_int_type_node);
  TYPE_NAME (bool_int_type_node) = tdecl;

  tdecl = add_builtin_type ("__pixel", pixel_type_node);
  TYPE_NAME (pixel_type_node) = tdecl;

  bool_V16QI_type_node = rs6000_vector_type ("__vector __bool char",
					     bool_char_type_node, 16);
  ptr_bool_V16QI_type_node
    = build_pointer_type (build_qualified_type (bool_V16QI_type_node,
						TYPE_QUAL_CONST));

  bool_V8HI_type_node = rs6000_vector_type ("__vector __bool short",
					    bool_short_type_node, 8);
  ptr_bool_V8HI_type_node
    = build_pointer_type (build_qualified_type (bool_V8HI_type_node,
						TYPE_QUAL_CONST));

  bool_V4SI_type_node = rs6000_vector_type ("__vector __bool int",
					    bool_int_type_node, 4);
  ptr_bool_V4SI_type_node
    = build_pointer_type (build_qualified_type (bool_V4SI_type_node,
						TYPE_QUAL_CONST));

  bool_V2DI_type_node = rs6000_vector_type (TARGET_POWERPC64
					    ? "__vector __bool long"
					    : "__vector __bool long long",
					    bool_long_long_type_node, 2);
  ptr_bool_V2DI_type_node
    = build_pointer_type (build_qualified_type (bool_V2DI_type_node,
						TYPE_QUAL_CONST));

  bool_V1TI_type_node = rs6000_vector_type ("__vector __bool __int128",
					    intTI_type_node, 1);
  ptr_bool_V1TI_type_node
    = build_pointer_type (build_qualified_type (bool_V1TI_type_node,
						TYPE_QUAL_CONST));

  pixel_V8HI_type_node = rs6000_vector_type ("__vector __pixel",
					     pixel_type_node, 8);
  ptr_pixel_V8HI_type_node
    = build_pointer_type (build_qualified_type (pixel_V8HI_type_node,
						TYPE_QUAL_CONST));
  pcvoid_type_node
    = build_pointer_type (build_qualified_type (void_type_node,
						TYPE_QUAL_CONST));

  /* Execute the autogenerated initialization code for builtins.  */
  rs6000_init_generated_builtins (rs6000_builtin_info_fntype,
  				  rs6000_instance_info_fntype,
				  rs6000_overload_info,
				  rs6000_builtin_decls);

  if (TARGET_DEBUG_BUILTIN)
    {
      fprintf (stderr, "\nAutogenerated built-in functions:\n\n");
      for (int i = 1; i < (int) RS6000_BIF_MAX; i++)
	{
	  enum rs6000_gen_builtins fn_code = (enum rs6000_gen_builtins) i;
	  if (!rs6000_builtin_is_supported (fn_code))
	    continue;
	  tree fntype = rs6000_builtin_info_fntype[i];
	  tree t = TREE_TYPE (fntype);
	  fprintf (stderr, "%s %s (", rs6000_type_string (t),
		   rs6000_builtin_info[i].bifname);
	  t = TYPE_ARG_TYPES (fntype);
	  while (t && TREE_VALUE (t) != void_type_node)
	    {
	      fprintf (stderr, "%s",
		       rs6000_type_string (TREE_VALUE (t)));
	      t = TREE_CHAIN (t);
	      if (t && TREE_VALUE (t) != void_type_node)
		fprintf (stderr, ", ");
	    }
	  fprintf (stderr, "); %s [%4d]\n",
		   rs6000_builtin_info[i].attr_string, (int) i);
	}
      fprintf (stderr, "\nEnd autogenerated built-in functions.\n\n\n");
     }

  if (TARGET_XCOFF)
    {
      /* AIX libm provides clog as __clog.  */
      if ((tdecl = builtin_decl_explicit (BUILT_IN_CLOG)) != NULL_TREE)
	set_user_assembler_name (tdecl, "__clog");

      /* When long double is 64 bit, some long double builtins of libc
	 functions (like __builtin_frexpl) must call the double version
	 (frexp) not the long double version (frexpl) that expects a 128 bit
	 argument.  */
      if (! TARGET_LONG_DOUBLE_128)
	{
	  if ((tdecl = builtin_decl_explicit (BUILT_IN_FMODL)) != NULL_TREE)
	    set_user_assembler_name (tdecl, "fmod");
	  if ((tdecl = builtin_decl_explicit (BUILT_IN_FREXPL)) != NULL_TREE)
	    set_user_assembler_name (tdecl, "frexp");
	  if ((tdecl = builtin_decl_explicit (BUILT_IN_LDEXPL)) != NULL_TREE)
	    set_user_assembler_name (tdecl, "ldexp");
	  if ((tdecl = builtin_decl_explicit (BUILT_IN_MODFL)) != NULL_TREE)
	    set_user_assembler_name (tdecl, "modf");
	}
    }

  altivec_builtin_mask_for_load
    = rs6000_builtin_decls[RS6000_BIF_MASK_FOR_LOAD];

#ifdef SUBTARGET_INIT_BUILTINS
  SUBTARGET_INIT_BUILTINS;
#endif

  return;
}

/* **** GIMPLE folding support **** */

/* Helper function to handle the gimple folding of a vector compare
   operation.  This sets up true/false vectors, and uses the
   VEC_COND_EXPR operation.
   CODE indicates which comparison is to be made. (EQ, GT, ...).
   TYPE indicates the type of the result.
   Code is inserted before GSI.  */
static tree
fold_build_vec_cmp (tree_code code, tree type, tree arg0, tree arg1,
		    gimple_stmt_iterator *gsi)
{
  tree cmp_type = truth_type_for (type);
  tree zero_vec = build_zero_cst (type);
  tree minus_one_vec = build_minus_one_cst (type);
  tree temp = create_tmp_reg_or_ssa_name (cmp_type);
  gimple *g = gimple_build_assign (temp, code, arg0, arg1);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  return fold_build3 (VEC_COND_EXPR, type, temp, minus_one_vec, zero_vec);
}

/* Helper function to handle the in-between steps for the
   vector compare built-ins.  */
static void
fold_compare_helper (gimple_stmt_iterator *gsi, tree_code code, gimple *stmt)
{
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  tree lhs = gimple_call_lhs (stmt);
  tree cmp = fold_build_vec_cmp (code, TREE_TYPE (lhs), arg0, arg1, gsi);
  gimple *g = gimple_build_assign (lhs, cmp);
  gimple_set_location (g, gimple_location (stmt));
  gsi_replace (gsi, g, true);
}

/* Helper function to map V2DF and V4SF types to their
 integral equivalents (V2DI and V4SI).  */
tree map_to_integral_tree_type (tree input_tree_type)
{
  if (INTEGRAL_TYPE_P (TREE_TYPE (input_tree_type)))
    return input_tree_type;
  else
    {
      if (types_compatible_p (TREE_TYPE (input_tree_type),
			      TREE_TYPE (V2DF_type_node)))
	return V2DI_type_node;
      else if (types_compatible_p (TREE_TYPE (input_tree_type),
				   TREE_TYPE (V4SF_type_node)))
	return V4SI_type_node;
      else
	gcc_unreachable ();
    }
}

/* Helper function to handle the vector merge[hl] built-ins.  The
   implementation difference between h and l versions for this code are in
   the values used when building of the permute vector for high word versus
   low word merge.  The variance is keyed off the use_high parameter.  */
static void
fold_mergehl_helper (gimple_stmt_iterator *gsi, gimple *stmt, int use_high)
{
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  tree lhs = gimple_call_lhs (stmt);
  tree lhs_type = TREE_TYPE (lhs);
  int n_elts = TYPE_VECTOR_SUBPARTS (lhs_type);
  int midpoint = n_elts / 2;
  int offset = 0;

  if (use_high == 1)
    offset = midpoint;

  /* The permute_type will match the lhs for integral types.  For double and
     float types, the permute type needs to map to the V2 or V4 type that
     matches size.  */
  tree permute_type;
  permute_type = map_to_integral_tree_type (lhs_type);
  tree_vector_builder elts (permute_type, VECTOR_CST_NELTS (arg0), 1);

  for (int i = 0; i < midpoint; i++)
    {
      elts.safe_push (build_int_cst (TREE_TYPE (permute_type),
				     offset + i));
      elts.safe_push (build_int_cst (TREE_TYPE (permute_type),
				     offset + n_elts + i));
    }

  tree permute = elts.build ();

  gimple *g = gimple_build_assign (lhs, VEC_PERM_EXPR, arg0, arg1, permute);
  gimple_set_location (g, gimple_location (stmt));
  gsi_replace (gsi, g, true);
}

/* Helper function to handle the vector merge[eo] built-ins.  */
static void
fold_mergeeo_helper (gimple_stmt_iterator *gsi, gimple *stmt, int use_odd)
{
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  tree lhs = gimple_call_lhs (stmt);
  tree lhs_type = TREE_TYPE (lhs);
  int n_elts = TYPE_VECTOR_SUBPARTS (lhs_type);

  /* The permute_type will match the lhs for integral types.  For double and
     float types, the permute type needs to map to the V2 or V4 type that
     matches size.  */
  tree permute_type;
  permute_type = map_to_integral_tree_type (lhs_type);

  tree_vector_builder elts (permute_type, VECTOR_CST_NELTS (arg0), 1);

 /* Build the permute vector.  */
  for (int i = 0; i < n_elts / 2; i++)
    {
      elts.safe_push (build_int_cst (TREE_TYPE (permute_type),
				     2*i + use_odd));
      elts.safe_push (build_int_cst (TREE_TYPE (permute_type),
				     2*i + use_odd + n_elts));
    }

  tree permute = elts.build ();

  gimple *g = gimple_build_assign (lhs, VEC_PERM_EXPR, arg0, arg1, permute);
  gimple_set_location (g, gimple_location (stmt));
  gsi_replace (gsi, g, true);
}

/*  Helper function to sort out which built-ins may be valid without having
    a LHS.  */
static bool
rs6000_builtin_valid_without_lhs (enum rs6000_gen_builtins fn_code,
				  tree fndecl)
{
  if (TREE_TYPE (TREE_TYPE (fndecl)) == void_type_node)
    return true;

  switch (fn_code)
    {
    case RS6000_BIF_STVX_V16QI:
    case RS6000_BIF_STVX_V8HI:
    case RS6000_BIF_STVX_V4SI:
    case RS6000_BIF_STVX_V4SF:
    case RS6000_BIF_STVX_V2DI:
    case RS6000_BIF_STVX_V2DF:
    case RS6000_BIF_STXVW4X_V16QI:
    case RS6000_BIF_STXVW4X_V8HI:
    case RS6000_BIF_STXVW4X_V4SF:
    case RS6000_BIF_STXVW4X_V4SI:
    case RS6000_BIF_STXVD2X_V2DF:
    case RS6000_BIF_STXVD2X_V2DI:
      return true;
    default:
      return false;
    }
}

/* Expand the MMA built-ins early, so that we can convert the pass-by-reference
   __vector_quad arguments into pass-by-value arguments, leading to more
   efficient code generation.  */
static bool
rs6000_gimple_fold_mma_builtin (gimple_stmt_iterator *gsi,
				rs6000_gen_builtins fn_code)
{
  gimple *stmt = gsi_stmt (*gsi);
  size_t fncode = (size_t) fn_code;

  if (!bif_is_mma (rs6000_builtin_info[fncode]))
    return false;

  /* Each call that can be gimple-expanded has an associated built-in
     function that it will expand into.  If this one doesn't, we have
     already expanded it!  Exceptions: lxvp and stxvp.  */
  if (rs6000_builtin_info[fncode].assoc_bif == RS6000_BIF_NONE
      && fncode != RS6000_BIF_LXVP
      && fncode != RS6000_BIF_STXVP)
    return false;

  bifdata *bd = &rs6000_builtin_info[fncode];
  unsigned nopnds = bd->nargs;
  gimple_seq new_seq = NULL;
  gimple *new_call;
  tree new_decl;

  /* Compatibility built-ins; we used to call these
     __builtin_mma_{dis,}assemble_pair, but now we call them
     __builtin_vsx_{dis,}assemble_pair.  Handle the old versions.  */
  if (fncode == RS6000_BIF_ASSEMBLE_PAIR)
    fncode = RS6000_BIF_ASSEMBLE_PAIR_V;
  else if (fncode == RS6000_BIF_DISASSEMBLE_PAIR)
    fncode = RS6000_BIF_DISASSEMBLE_PAIR_V;

  if (fncode == RS6000_BIF_DISASSEMBLE_ACC
      || fncode == RS6000_BIF_DISASSEMBLE_PAIR_V)
    {
      /* This is an MMA disassemble built-in function.  */
      push_gimplify_context (true);
      unsigned nvec = (fncode == RS6000_BIF_DISASSEMBLE_ACC) ? 4 : 2;
      tree dst_ptr = gimple_call_arg (stmt, 0);
      tree src_ptr = gimple_call_arg (stmt, 1);
      tree src_type = (fncode == RS6000_BIF_DISASSEMBLE_ACC)
		      ? build_pointer_type (vector_quad_type_node)
		      : build_pointer_type (vector_pair_type_node);
      if (TREE_TYPE (src_ptr) != src_type)
	src_ptr = build1 (NOP_EXPR, src_type, src_ptr);

      tree src = create_tmp_reg_or_ssa_name (TREE_TYPE (src_type));
      gimplify_assign (src, build_simple_mem_ref (src_ptr), &new_seq);

      /* If we are not disassembling an accumulator/pair or our destination is
	 another accumulator/pair, then just copy the entire thing as is.  */
      if ((fncode == RS6000_BIF_DISASSEMBLE_ACC
	   && TREE_TYPE (TREE_TYPE (dst_ptr)) == vector_quad_type_node)
	  || (fncode == RS6000_BIF_DISASSEMBLE_PAIR_V
	      && TREE_TYPE (TREE_TYPE (dst_ptr)) == vector_pair_type_node))
	{
	  tree dst = build_simple_mem_ref (build1 (NOP_EXPR,
						   src_type, dst_ptr));
	  gimplify_assign (dst, src, &new_seq);
	  pop_gimplify_context (NULL);
	  gsi_replace_with_seq (gsi, new_seq, true);
	  return true;
	}

      /* If we're disassembling an accumulator into a different type, we need
	 to emit a xxmfacc instruction now, since we cannot do it later.  */
      if (fncode == RS6000_BIF_DISASSEMBLE_ACC)
	{
	  new_decl = rs6000_builtin_decls[RS6000_BIF_XXMFACC_INTERNAL];
	  new_call = gimple_build_call (new_decl, 1, src);
	  src = create_tmp_reg_or_ssa_name (vector_quad_type_node);
	  gimple_call_set_lhs (new_call, src);
	  gimple_seq_add_stmt (&new_seq, new_call);
	}

      /* Copy the accumulator/pair vector by vector.  */
      new_decl
	= rs6000_builtin_decls[rs6000_builtin_info[fncode].assoc_bif];
      tree dst_type = build_pointer_type_for_mode (unsigned_V16QI_type_node,
						   ptr_mode, true);
      tree dst_base = build1 (NOP_EXPR, dst_type, dst_ptr);
      for (unsigned i = 0; i < nvec; i++)
	{
	  unsigned index = WORDS_BIG_ENDIAN ? i : nvec - 1 - i;
	  tree dst = build2 (MEM_REF, unsigned_V16QI_type_node, dst_base,
			     build_int_cst (dst_type, index * 16));
	  tree dstssa = create_tmp_reg_or_ssa_name (unsigned_V16QI_type_node);
	  new_call = gimple_build_call (new_decl, 2, src,
					build_int_cstu (uint16_type_node, i));
	  gimple_call_set_lhs (new_call, dstssa);
	  gimple_seq_add_stmt (&new_seq, new_call);
	  gimplify_assign (dst, dstssa, &new_seq);
	}
      pop_gimplify_context (NULL);
      gsi_replace_with_seq (gsi, new_seq, true);
      return true;
    }

  /* TODO: Do some factoring on these two chunks.  */
  if (fncode == RS6000_BIF_LXVP)
    {
      push_gimplify_context (true);
      tree offset = gimple_call_arg (stmt, 0);
      tree ptr = gimple_call_arg (stmt, 1);
      tree lhs = gimple_call_lhs (stmt);
      if (TREE_TYPE (TREE_TYPE (ptr)) != vector_pair_type_node)
	ptr = build1 (NOP_EXPR,
		      build_pointer_type (vector_pair_type_node), ptr);
      tree mem = build_simple_mem_ref (build2 (POINTER_PLUS_EXPR,
					       TREE_TYPE (ptr), ptr, offset));
      gimplify_assign (lhs, mem, &new_seq);
      pop_gimplify_context (NULL);
      gsi_replace_with_seq (gsi, new_seq, true);
      return true;
    }

  if (fncode == RS6000_BIF_STXVP)
    {
      push_gimplify_context (true);
      tree src = gimple_call_arg (stmt, 0);
      tree offset = gimple_call_arg (stmt, 1);
      tree ptr = gimple_call_arg (stmt, 2);
      if (TREE_TYPE (TREE_TYPE (ptr)) != vector_pair_type_node)
	ptr = build1 (NOP_EXPR,
		      build_pointer_type (vector_pair_type_node), ptr);
      tree mem = build_simple_mem_ref (build2 (POINTER_PLUS_EXPR,
					       TREE_TYPE (ptr), ptr, offset));
      gimplify_assign (mem, src, &new_seq);
      pop_gimplify_context (NULL);
      gsi_replace_with_seq (gsi, new_seq, true);
      return true;
    }

  /* Convert this built-in into an internal version that uses pass-by-value
     arguments.  The internal built-in is found in the assoc_bif field.  */
  new_decl = rs6000_builtin_decls[rs6000_builtin_info[fncode].assoc_bif];
  tree lhs, op[MAX_MMA_OPERANDS];
  tree acc = gimple_call_arg (stmt, 0);
  push_gimplify_context (true);

  if (bif_is_quad (*bd))
    {
      /* This built-in has a pass-by-reference accumulator input, so load it
	 into a temporary accumulator for use as a pass-by-value input.  */
      op[0] = create_tmp_reg_or_ssa_name (vector_quad_type_node);
      for (unsigned i = 1; i < nopnds; i++)
	op[i] = gimple_call_arg (stmt, i);
      gimplify_assign (op[0], build_simple_mem_ref (acc), &new_seq);
    }
  else
    {
      /* This built-in does not use its pass-by-reference accumulator argument
	 as an input argument, so remove it from the input list.  */
      nopnds--;
      for (unsigned i = 0; i < nopnds; i++)
	op[i] = gimple_call_arg (stmt, i + 1);
    }

  switch (nopnds)
    {
    case 0:
      new_call = gimple_build_call (new_decl, 0);
      break;
    case 1:
      new_call = gimple_build_call (new_decl, 1, op[0]);
      break;
    case 2:
      new_call = gimple_build_call (new_decl, 2, op[0], op[1]);
      break;
    case 3:
      new_call = gimple_build_call (new_decl, 3, op[0], op[1], op[2]);
      break;
    case 4:
      new_call = gimple_build_call (new_decl, 4, op[0], op[1], op[2], op[3]);
      break;
    case 5:
      new_call = gimple_build_call (new_decl, 5, op[0], op[1], op[2], op[3],
				    op[4]);
      break;
    case 6:
      new_call = gimple_build_call (new_decl, 6, op[0], op[1], op[2], op[3],
				    op[4], op[5]);
      break;
    case 7:
      new_call = gimple_build_call (new_decl, 7, op[0], op[1], op[2], op[3],
				    op[4], op[5], op[6]);
      break;
    default:
      gcc_unreachable ();
    }

  if (fncode == RS6000_BIF_BUILD_PAIR || fncode == RS6000_BIF_ASSEMBLE_PAIR_V)
    lhs = create_tmp_reg_or_ssa_name (vector_pair_type_node);
  else
    lhs = create_tmp_reg_or_ssa_name (vector_quad_type_node);
  gimple_call_set_lhs (new_call, lhs);
  gimple_seq_add_stmt (&new_seq, new_call);
  gimplify_assign (build_simple_mem_ref (acc), lhs, &new_seq);
  pop_gimplify_context (NULL);
  gsi_replace_with_seq (gsi, new_seq, true);

  return true;
}

/* Fold a machine-dependent built-in in GIMPLE.  (For folding into
   a constant, use rs6000_fold_builtin.)  */
bool
rs6000_gimple_fold_builtin (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  tree fndecl = gimple_call_fndecl (stmt);
  gcc_checking_assert (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD);
  enum rs6000_gen_builtins fn_code
    = (enum rs6000_gen_builtins) DECL_MD_FUNCTION_CODE (fndecl);
  tree arg0, arg1, lhs, temp;
  enum tree_code bcode;
  gimple *g;

  /* For an unresolved overloaded builtin, return early here since there
     is no builtin info for it and we are unable to fold it.  */
  if (fn_code > RS6000_OVLD_NONE)
    return false;

  size_t uns_fncode = (size_t) fn_code;
  enum insn_code icode = rs6000_builtin_info[uns_fncode].icode;
  const char *fn_name1 = rs6000_builtin_info[uns_fncode].bifname;
  const char *fn_name2 = (icode != CODE_FOR_nothing)
			  ? get_insn_name ((int) icode)
			  : "nothing";

  if (TARGET_DEBUG_BUILTIN)
      fprintf (stderr, "rs6000_gimple_fold_builtin %d %s %s\n",
	       fn_code, fn_name1, fn_name2);

  /* Prevent gimple folding for code that does not have a LHS, unless it is
     allowed per the rs6000_builtin_valid_without_lhs helper function.  */
  if (!gimple_call_lhs (stmt)
      && !rs6000_builtin_valid_without_lhs (fn_code, fndecl))
    return false;

  /* Don't fold invalid builtins, let rs6000_expand_builtin diagnose it.  */
  if (!rs6000_builtin_is_supported (fn_code))
    return false;

  if (rs6000_gimple_fold_mma_builtin (gsi, fn_code))
    return true;

  switch (fn_code)
    {
    /* Flavors of vec_add.  We deliberately don't expand
       RS6000_BIF_VADDUQM as it gets lowered from V1TImode to
       TImode, resulting in much poorer code generation.  */
    case RS6000_BIF_VADDUBM:
    case RS6000_BIF_VADDUHM:
    case RS6000_BIF_VADDUWM:
    case RS6000_BIF_VADDUDM:
    case RS6000_BIF_VADDFP:
    case RS6000_BIF_XVADDDP:
    case RS6000_BIF_XVADDSP:
      bcode = PLUS_EXPR;
    do_binary:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      if (INTEGRAL_TYPE_P (TREE_TYPE (TREE_TYPE (lhs)))
	  && !TYPE_OVERFLOW_WRAPS (TREE_TYPE (TREE_TYPE (lhs))))
	{
	  /* Ensure the binary operation is performed in a type
	     that wraps if it is integral type.  */
	  gimple_seq stmts = NULL;
	  tree type = unsigned_type_for (TREE_TYPE (lhs));
	  tree uarg0 = gimple_build (&stmts, VIEW_CONVERT_EXPR,
				     type, arg0);
	  tree uarg1 = gimple_build (&stmts, VIEW_CONVERT_EXPR,
				     type, arg1);
	  tree res = gimple_build (&stmts, gimple_location (stmt), bcode,
				   type, uarg0, uarg1);
	  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	  g = gimple_build_assign (lhs, VIEW_CONVERT_EXPR,
				   build1 (VIEW_CONVERT_EXPR,
					   TREE_TYPE (lhs), res));
	  gsi_replace (gsi, g, true);
	  return true;
	}
      g = gimple_build_assign (lhs, bcode, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Flavors of vec_sub.  We deliberately don't expand
       RS6000_BIF_VSUBUQM. */
    case RS6000_BIF_VSUBUBM:
    case RS6000_BIF_VSUBUHM:
    case RS6000_BIF_VSUBUWM:
    case RS6000_BIF_VSUBUDM:
    case RS6000_BIF_VSUBFP:
    case RS6000_BIF_XVSUBDP:
    case RS6000_BIF_XVSUBSP:
      bcode = MINUS_EXPR;
      goto do_binary;
    case RS6000_BIF_XVMULSP:
    case RS6000_BIF_XVMULDP:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, MULT_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Even element flavors of vec_mul (signed). */
    case RS6000_BIF_VMULESB:
    case RS6000_BIF_VMULESH:
    case RS6000_BIF_VMULESW:
    /* Even element flavors of vec_mul (unsigned).  */
    case RS6000_BIF_VMULEUB:
    case RS6000_BIF_VMULEUH:
    case RS6000_BIF_VMULEUW:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, VEC_WIDEN_MULT_EVEN_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Odd element flavors of vec_mul (signed).  */
    case RS6000_BIF_VMULOSB:
    case RS6000_BIF_VMULOSH:
    case RS6000_BIF_VMULOSW:
    /* Odd element flavors of vec_mul (unsigned). */
    case RS6000_BIF_VMULOUB:
    case RS6000_BIF_VMULOUH:
    case RS6000_BIF_VMULOUW:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, VEC_WIDEN_MULT_ODD_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Flavors of vec_div (Integer).  */
    case RS6000_BIF_DIV_V2DI:
    case RS6000_BIF_UDIV_V2DI:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, TRUNC_DIV_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Flavors of vec_div (Float).  */
    case RS6000_BIF_XVDIVSP:
    case RS6000_BIF_XVDIVDP:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, RDIV_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Flavors of vec_and.  */
    case RS6000_BIF_VAND_V16QI_UNS:
    case RS6000_BIF_VAND_V16QI:
    case RS6000_BIF_VAND_V8HI_UNS:
    case RS6000_BIF_VAND_V8HI:
    case RS6000_BIF_VAND_V4SI_UNS:
    case RS6000_BIF_VAND_V4SI:
    case RS6000_BIF_VAND_V2DI_UNS:
    case RS6000_BIF_VAND_V2DI:
    case RS6000_BIF_VAND_V4SF:
    case RS6000_BIF_VAND_V2DF:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, BIT_AND_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Flavors of vec_andc.  */
    case RS6000_BIF_VANDC_V16QI_UNS:
    case RS6000_BIF_VANDC_V16QI:
    case RS6000_BIF_VANDC_V8HI_UNS:
    case RS6000_BIF_VANDC_V8HI:
    case RS6000_BIF_VANDC_V4SI_UNS:
    case RS6000_BIF_VANDC_V4SI:
    case RS6000_BIF_VANDC_V2DI_UNS:
    case RS6000_BIF_VANDC_V2DI:
    case RS6000_BIF_VANDC_V4SF:
    case RS6000_BIF_VANDC_V2DF:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      temp = create_tmp_reg_or_ssa_name (TREE_TYPE (arg1));
      g = gimple_build_assign (temp, BIT_NOT_EXPR, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      g = gimple_build_assign (lhs, BIT_AND_EXPR, arg0, temp);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Flavors of vec_nand.  */
    case RS6000_BIF_NAND_V16QI_UNS:
    case RS6000_BIF_NAND_V16QI:
    case RS6000_BIF_NAND_V8HI_UNS:
    case RS6000_BIF_NAND_V8HI:
    case RS6000_BIF_NAND_V4SI_UNS:
    case RS6000_BIF_NAND_V4SI:
    case RS6000_BIF_NAND_V2DI_UNS:
    case RS6000_BIF_NAND_V2DI:
    case RS6000_BIF_NAND_V4SF:
    case RS6000_BIF_NAND_V2DF:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      temp = create_tmp_reg_or_ssa_name (TREE_TYPE (arg1));
      g = gimple_build_assign (temp, BIT_AND_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      g = gimple_build_assign (lhs, BIT_NOT_EXPR, temp);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Flavors of vec_or.  */
    case RS6000_BIF_VOR_V16QI_UNS:
    case RS6000_BIF_VOR_V16QI:
    case RS6000_BIF_VOR_V8HI_UNS:
    case RS6000_BIF_VOR_V8HI:
    case RS6000_BIF_VOR_V4SI_UNS:
    case RS6000_BIF_VOR_V4SI:
    case RS6000_BIF_VOR_V2DI_UNS:
    case RS6000_BIF_VOR_V2DI:
    case RS6000_BIF_VOR_V4SF:
    case RS6000_BIF_VOR_V2DF:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, BIT_IOR_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* flavors of vec_orc.  */
    case RS6000_BIF_ORC_V16QI_UNS:
    case RS6000_BIF_ORC_V16QI:
    case RS6000_BIF_ORC_V8HI_UNS:
    case RS6000_BIF_ORC_V8HI:
    case RS6000_BIF_ORC_V4SI_UNS:
    case RS6000_BIF_ORC_V4SI:
    case RS6000_BIF_ORC_V2DI_UNS:
    case RS6000_BIF_ORC_V2DI:
    case RS6000_BIF_ORC_V4SF:
    case RS6000_BIF_ORC_V2DF:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      temp = create_tmp_reg_or_ssa_name (TREE_TYPE (arg1));
      g = gimple_build_assign (temp, BIT_NOT_EXPR, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      g = gimple_build_assign (lhs, BIT_IOR_EXPR, arg0, temp);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Flavors of vec_xor.  */
    case RS6000_BIF_VXOR_V16QI_UNS:
    case RS6000_BIF_VXOR_V16QI:
    case RS6000_BIF_VXOR_V8HI_UNS:
    case RS6000_BIF_VXOR_V8HI:
    case RS6000_BIF_VXOR_V4SI_UNS:
    case RS6000_BIF_VXOR_V4SI:
    case RS6000_BIF_VXOR_V2DI_UNS:
    case RS6000_BIF_VXOR_V2DI:
    case RS6000_BIF_VXOR_V4SF:
    case RS6000_BIF_VXOR_V2DF:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, BIT_XOR_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Flavors of vec_nor.  */
    case RS6000_BIF_VNOR_V16QI_UNS:
    case RS6000_BIF_VNOR_V16QI:
    case RS6000_BIF_VNOR_V8HI_UNS:
    case RS6000_BIF_VNOR_V8HI:
    case RS6000_BIF_VNOR_V4SI_UNS:
    case RS6000_BIF_VNOR_V4SI:
    case RS6000_BIF_VNOR_V2DI_UNS:
    case RS6000_BIF_VNOR_V2DI:
    case RS6000_BIF_VNOR_V4SF:
    case RS6000_BIF_VNOR_V2DF:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      temp = create_tmp_reg_or_ssa_name (TREE_TYPE (arg1));
      g = gimple_build_assign (temp, BIT_IOR_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      g = gimple_build_assign (lhs, BIT_NOT_EXPR, temp);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* flavors of vec_abs.  */
    case RS6000_BIF_ABS_V16QI:
    case RS6000_BIF_ABS_V8HI:
    case RS6000_BIF_ABS_V4SI:
    case RS6000_BIF_ABS_V4SF:
    case RS6000_BIF_ABS_V2DI:
    case RS6000_BIF_XVABSDP:
    case RS6000_BIF_XVABSSP:
      arg0 = gimple_call_arg (stmt, 0);
      if (INTEGRAL_TYPE_P (TREE_TYPE (TREE_TYPE (arg0)))
	  && !TYPE_OVERFLOW_WRAPS (TREE_TYPE (TREE_TYPE (arg0))))
	return false;
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, ABS_EXPR, arg0);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* fold into MIN_EXPR when fast-math is set.  */
    case RS6000_BIF_XSMINDP:
    /* flavors of vec_min.  */
    case RS6000_BIF_XVMINDP:
    case RS6000_BIF_XVMINSP:
    case RS6000_BIF_VMINFP:
      {
	lhs = gimple_call_lhs (stmt);
	tree type = TREE_TYPE (lhs);
	if (HONOR_NANS (type))
	  return false;
	gcc_fallthrough ();
      }
    case RS6000_BIF_VMINSD:
    case RS6000_BIF_VMINUD:
    case RS6000_BIF_VMINSB:
    case RS6000_BIF_VMINSH:
    case RS6000_BIF_VMINSW:
    case RS6000_BIF_VMINUB:
    case RS6000_BIF_VMINUH:
    case RS6000_BIF_VMINUW:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, MIN_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* fold into MAX_EXPR when fast-math is set.  */
    case RS6000_BIF_XSMAXDP:
    /* flavors of vec_max.  */
    case RS6000_BIF_XVMAXDP:
    case RS6000_BIF_XVMAXSP:
    case RS6000_BIF_VMAXFP:
      {
	lhs = gimple_call_lhs (stmt);
	tree type = TREE_TYPE (lhs);
	if (HONOR_NANS (type))
	  return false;
	gcc_fallthrough ();
      }
    case RS6000_BIF_VMAXSD:
    case RS6000_BIF_VMAXUD:
    case RS6000_BIF_VMAXSB:
    case RS6000_BIF_VMAXSH:
    case RS6000_BIF_VMAXSW:
    case RS6000_BIF_VMAXUB:
    case RS6000_BIF_VMAXUH:
    case RS6000_BIF_VMAXUW:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, MAX_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Flavors of vec_eqv.  */
    case RS6000_BIF_EQV_V16QI:
    case RS6000_BIF_EQV_V8HI:
    case RS6000_BIF_EQV_V4SI:
    case RS6000_BIF_EQV_V4SF:
    case RS6000_BIF_EQV_V2DF:
    case RS6000_BIF_EQV_V2DI:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      temp = create_tmp_reg_or_ssa_name (TREE_TYPE (arg1));
      g = gimple_build_assign (temp, BIT_XOR_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      g = gimple_build_assign (lhs, BIT_NOT_EXPR, temp);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
    /* Flavors of vec_rotate_left.  */
    case RS6000_BIF_VRLB:
    case RS6000_BIF_VRLH:
    case RS6000_BIF_VRLW:
    case RS6000_BIF_VRLD:
      arg0 = gimple_call_arg (stmt, 0);
      arg1 = gimple_call_arg (stmt, 1);
      lhs = gimple_call_lhs (stmt);
      g = gimple_build_assign (lhs, LROTATE_EXPR, arg0, arg1);
      gimple_set_location (g, gimple_location (stmt));
      gsi_replace (gsi, g, true);
      return true;
  /* Flavors of vector shift right algebraic.
     vec_sra{b,h,w} -> vsra{b,h,w}.  */
    case RS6000_BIF_VSRAB:
    case RS6000_BIF_VSRAH:
    case RS6000_BIF_VSRAW:
    case RS6000_BIF_VSRAD:
      {
	arg0 = gimple_call_arg (stmt, 0);
	arg1 = gimple_call_arg (stmt, 1);
	lhs = gimple_call_lhs (stmt);
	tree arg1_type = TREE_TYPE (arg1);
	tree unsigned_arg1_type = unsigned_type_for (TREE_TYPE (arg1));
	tree unsigned_element_type = unsigned_type_for (TREE_TYPE (arg1_type));
	location_t loc = gimple_location (stmt);
	/* Force arg1 into the range valid matching the arg0 type.  */
	/* Build a vector consisting of the max valid bit-size values.  */
	int n_elts = VECTOR_CST_NELTS (arg1);
	tree element_size = build_int_cst (unsigned_element_type,
					   128 / n_elts);
	tree_vector_builder elts (unsigned_arg1_type, n_elts, 1);
	for (int i = 0; i < n_elts; i++)
	  elts.safe_push (element_size);
	tree modulo_tree = elts.build ();
	/* Modulo the provided shift value against that vector.  */
	gimple_seq stmts = NULL;
	tree unsigned_arg1 = gimple_build (&stmts, VIEW_CONVERT_EXPR,
					   unsigned_arg1_type, arg1);
	tree new_arg1 = gimple_build (&stmts, loc, TRUNC_MOD_EXPR,
				      unsigned_arg1_type, unsigned_arg1,
				      modulo_tree);
	gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	/* And finally, do the shift.  */
	g = gimple_build_assign (lhs, RSHIFT_EXPR, arg0, new_arg1);
	gimple_set_location (g, loc);
	gsi_replace (gsi, g, true);
	return true;
      }
   /* Flavors of vector shift left.
      builtin_altivec_vsl{b,h,w} -> vsl{b,h,w}.  */
    case RS6000_BIF_VSLB:
    case RS6000_BIF_VSLH:
    case RS6000_BIF_VSLW:
    case RS6000_BIF_VSLD:
      {
	location_t loc;
	gimple_seq stmts = NULL;
	arg0 = gimple_call_arg (stmt, 0);
	tree arg0_type = TREE_TYPE (arg0);
	if (INTEGRAL_TYPE_P (TREE_TYPE (arg0_type))
	    && !TYPE_OVERFLOW_WRAPS (TREE_TYPE (arg0_type)))
	  return false;
	arg1 = gimple_call_arg (stmt, 1);
	tree arg1_type = TREE_TYPE (arg1);
	tree unsigned_arg1_type = unsigned_type_for (TREE_TYPE (arg1));
	tree unsigned_element_type = unsigned_type_for (TREE_TYPE (arg1_type));
	loc = gimple_location (stmt);
	lhs = gimple_call_lhs (stmt);
	/* Force arg1 into the range valid matching the arg0 type.  */
	/* Build a vector consisting of the max valid bit-size values.  */
	int n_elts = VECTOR_CST_NELTS (arg1);
	int tree_size_in_bits = TREE_INT_CST_LOW (size_in_bytes (arg1_type))
				* BITS_PER_UNIT;
	tree element_size = build_int_cst (unsigned_element_type,
					   tree_size_in_bits / n_elts);
	tree_vector_builder elts (unsigned_type_for (arg1_type), n_elts, 1);
	for (int i = 0; i < n_elts; i++)
	  elts.safe_push (element_size);
	tree modulo_tree = elts.build ();
	/* Modulo the provided shift value against that vector.  */
	tree unsigned_arg1 = gimple_build (&stmts, VIEW_CONVERT_EXPR,
					   unsigned_arg1_type, arg1);
	tree new_arg1 = gimple_build (&stmts, loc, TRUNC_MOD_EXPR,
				      unsigned_arg1_type, unsigned_arg1,
				      modulo_tree);
	gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	/* And finally, do the shift.  */
	g = gimple_build_assign (lhs, LSHIFT_EXPR, arg0, new_arg1);
	gimple_set_location (g, gimple_location (stmt));
	gsi_replace (gsi, g, true);
	return true;
      }
    /* Flavors of vector shift right.  */
    case RS6000_BIF_VSRB:
    case RS6000_BIF_VSRH:
    case RS6000_BIF_VSRW:
    case RS6000_BIF_VSRD:
      {
	arg0 = gimple_call_arg (stmt, 0);
	arg1 = gimple_call_arg (stmt, 1);
	lhs = gimple_call_lhs (stmt);
	tree arg1_type = TREE_TYPE (arg1);
	tree unsigned_arg1_type = unsigned_type_for (TREE_TYPE (arg1));
	tree unsigned_element_type = unsigned_type_for (TREE_TYPE (arg1_type));
	location_t loc = gimple_location (stmt);
	gimple_seq stmts = NULL;
	/* Convert arg0 to unsigned.  */
	tree arg0_unsigned
	  = gimple_build (&stmts, VIEW_CONVERT_EXPR,
			  unsigned_type_for (TREE_TYPE (arg0)), arg0);
	/* Force arg1 into the range valid matching the arg0 type.  */
	/* Build a vector consisting of the max valid bit-size values.  */
	int n_elts = VECTOR_CST_NELTS (arg1);
	tree element_size = build_int_cst (unsigned_element_type,
					   128 / n_elts);
	tree_vector_builder elts (unsigned_arg1_type, n_elts, 1);
	for (int i = 0; i < n_elts; i++)
	  elts.safe_push (element_size);
	tree modulo_tree = elts.build ();
	/* Modulo the provided shift value against that vector.  */
	tree unsigned_arg1 = gimple_build (&stmts, VIEW_CONVERT_EXPR,
					   unsigned_arg1_type, arg1);
	tree new_arg1 = gimple_build (&stmts, loc, TRUNC_MOD_EXPR,
				      unsigned_arg1_type, unsigned_arg1,
				      modulo_tree);
	/* Do the shift.  */
	tree res
	  = gimple_build (&stmts, RSHIFT_EXPR,
			  TREE_TYPE (arg0_unsigned), arg0_unsigned, new_arg1);
	/* Convert result back to the lhs type.  */
	res = gimple_build (&stmts, VIEW_CONVERT_EXPR, TREE_TYPE (lhs), res);
	gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	replace_call_with_value (gsi, res);
	return true;
      }
    /* Vector loads.  */
    case RS6000_BIF_LVX_V16QI:
    case RS6000_BIF_LVX_V8HI:
    case RS6000_BIF_LVX_V4SI:
    case RS6000_BIF_LVX_V4SF:
    case RS6000_BIF_LVX_V2DI:
    case RS6000_BIF_LVX_V2DF:
    case RS6000_BIF_LVX_V1TI:
      {
	arg0 = gimple_call_arg (stmt, 0);  // offset
	arg1 = gimple_call_arg (stmt, 1);  // address
	lhs = gimple_call_lhs (stmt);
	location_t loc = gimple_location (stmt);
	/* Since arg1 may be cast to a different type, just use ptr_type_node
	   here instead of trying to enforce TBAA on pointer types.  */
	tree arg1_type = ptr_type_node;
	tree lhs_type = TREE_TYPE (lhs);
	/* POINTER_PLUS_EXPR wants the offset to be of type 'sizetype'.  Create
	   the tree using the value from arg0.  The resulting type will match
	   the type of arg1.  */
	gimple_seq stmts = NULL;
	tree temp_offset = gimple_convert (&stmts, loc, sizetype, arg0);
	tree temp_addr = gimple_build (&stmts, loc, POINTER_PLUS_EXPR,
				       arg1_type, arg1, temp_offset);
	/* Mask off any lower bits from the address.  */
	tree aligned_addr = gimple_build (&stmts, loc, BIT_AND_EXPR,
					  arg1_type, temp_addr,
					  build_int_cst (arg1_type, -16));
	gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	if (!is_gimple_mem_ref_addr (aligned_addr))
	  {
	    tree t = make_ssa_name (TREE_TYPE (aligned_addr));
	    gimple *g = gimple_build_assign (t, aligned_addr);
	    gsi_insert_before (gsi, g, GSI_SAME_STMT);
	    aligned_addr = t;
	  }
	/* Use the build2 helper to set up the mem_ref.  The MEM_REF could also
	   take an offset, but since we've already incorporated the offset
	   above, here we just pass in a zero.  */
	gimple *g
	  = gimple_build_assign (lhs, build2 (MEM_REF, lhs_type, aligned_addr,
					      build_int_cst (arg1_type, 0)));
	gimple_set_location (g, loc);
	gsi_replace (gsi, g, true);
	return true;
      }
    /* Vector stores.  */
    case RS6000_BIF_STVX_V16QI:
    case RS6000_BIF_STVX_V8HI:
    case RS6000_BIF_STVX_V4SI:
    case RS6000_BIF_STVX_V4SF:
    case RS6000_BIF_STVX_V2DI:
    case RS6000_BIF_STVX_V2DF:
      {
	arg0 = gimple_call_arg (stmt, 0); /* Value to be stored.  */
	arg1 = gimple_call_arg (stmt, 1); /* Offset.  */
	tree arg2 = gimple_call_arg (stmt, 2); /* Store-to address.  */
	location_t loc = gimple_location (stmt);
	tree arg0_type = TREE_TYPE (arg0);
	/* Use ptr_type_node (no TBAA) for the arg2_type.
	   FIXME: (Richard)  "A proper fix would be to transition this type as
	   seen from the frontend to GIMPLE, for example in a similar way we
	   do for MEM_REFs by piggy-backing that on an extra argument, a
	   constant zero pointer of the alias pointer type to use (which would
	   also serve as a type indicator of the store itself).  I'd use a
	   target specific internal function for this (not sure if we can have
	   those target specific, but I guess if it's folded away then that's
	   fine) and get away with the overload set."  */
	tree arg2_type = ptr_type_node;
	/* POINTER_PLUS_EXPR wants the offset to be of type 'sizetype'.  Create
	   the tree using the value from arg0.  The resulting type will match
	   the type of arg2.  */
	gimple_seq stmts = NULL;
	tree temp_offset = gimple_convert (&stmts, loc, sizetype, arg1);
	tree temp_addr = gimple_build (&stmts, loc, POINTER_PLUS_EXPR,
				       arg2_type, arg2, temp_offset);
	/* Mask off any lower bits from the address.  */
	tree aligned_addr = gimple_build (&stmts, loc, BIT_AND_EXPR,
					  arg2_type, temp_addr,
					  build_int_cst (arg2_type, -16));
	gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	if (!is_gimple_mem_ref_addr (aligned_addr))
	  {
	    tree t = make_ssa_name (TREE_TYPE (aligned_addr));
	    gimple *g = gimple_build_assign (t, aligned_addr);
	    gsi_insert_before (gsi, g, GSI_SAME_STMT);
	    aligned_addr = t;
	  }
	/* The desired gimple result should be similar to:
	   MEM[(__vector floatD.1407 *)_1] = vf1D.2697;  */
	gimple *g
	  = gimple_build_assign (build2 (MEM_REF, arg0_type, aligned_addr,
					 build_int_cst (arg2_type, 0)), arg0);
	gimple_set_location (g, loc);
	gsi_replace (gsi, g, true);
	return true;
      }

    /* unaligned Vector loads.  */
    case RS6000_BIF_LXVW4X_V16QI:
    case RS6000_BIF_LXVW4X_V8HI:
    case RS6000_BIF_LXVW4X_V4SF:
    case RS6000_BIF_LXVW4X_V4SI:
    case RS6000_BIF_LXVD2X_V2DF:
    case RS6000_BIF_LXVD2X_V2DI:
      {
	arg0 = gimple_call_arg (stmt, 0);  // offset
	arg1 = gimple_call_arg (stmt, 1);  // address
	lhs = gimple_call_lhs (stmt);
	location_t loc = gimple_location (stmt);
	/* Since arg1 may be cast to a different type, just use ptr_type_node
	   here instead of trying to enforce TBAA on pointer types.  */
	tree arg1_type = ptr_type_node;
	tree lhs_type = TREE_TYPE (lhs);
	/* In GIMPLE the type of the MEM_REF specifies the alignment.  The
	  required alignment (power) is 4 bytes regardless of data type.  */
	tree align_ltype = build_aligned_type (lhs_type, 32);
	/* POINTER_PLUS_EXPR wants the offset to be of type 'sizetype'.  Create
	   the tree using the value from arg0.  The resulting type will match
	   the type of arg1.  */
	gimple_seq stmts = NULL;
	tree temp_offset = gimple_convert (&stmts, loc, sizetype, arg0);
	tree temp_addr = gimple_build (&stmts, loc, POINTER_PLUS_EXPR,
				       arg1_type, arg1, temp_offset);
	gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	if (!is_gimple_mem_ref_addr (temp_addr))
	  {
	    tree t = make_ssa_name (TREE_TYPE (temp_addr));
	    gimple *g = gimple_build_assign (t, temp_addr);
	    gsi_insert_before (gsi, g, GSI_SAME_STMT);
	    temp_addr = t;
	  }
	/* Use the build2 helper to set up the mem_ref.  The MEM_REF could also
	   take an offset, but since we've already incorporated the offset
	   above, here we just pass in a zero.  */
	gimple *g;
	g = gimple_build_assign (lhs, build2 (MEM_REF, align_ltype, temp_addr,
					      build_int_cst (arg1_type, 0)));
	gimple_set_location (g, loc);
	gsi_replace (gsi, g, true);
	return true;
      }

    /* unaligned Vector stores.  */
    case RS6000_BIF_STXVW4X_V16QI:
    case RS6000_BIF_STXVW4X_V8HI:
    case RS6000_BIF_STXVW4X_V4SF:
    case RS6000_BIF_STXVW4X_V4SI:
    case RS6000_BIF_STXVD2X_V2DF:
    case RS6000_BIF_STXVD2X_V2DI:
      {
	arg0 = gimple_call_arg (stmt, 0); /* Value to be stored.  */
	arg1 = gimple_call_arg (stmt, 1); /* Offset.  */
	tree arg2 = gimple_call_arg (stmt, 2); /* Store-to address.  */
	location_t loc = gimple_location (stmt);
	tree arg0_type = TREE_TYPE (arg0);
	/* Use ptr_type_node (no TBAA) for the arg2_type.  */
	tree arg2_type = ptr_type_node;
	/* In GIMPLE the type of the MEM_REF specifies the alignment.  The
	   required alignment (power) is 4 bytes regardless of data type.  */
	tree align_stype = build_aligned_type (arg0_type, 32);
	/* POINTER_PLUS_EXPR wants the offset to be of type 'sizetype'.  Create
	   the tree using the value from arg1.  */
	gimple_seq stmts = NULL;
	tree temp_offset = gimple_convert (&stmts, loc, sizetype, arg1);
	tree temp_addr = gimple_build (&stmts, loc, POINTER_PLUS_EXPR,
				       arg2_type, arg2, temp_offset);
	gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	if (!is_gimple_mem_ref_addr (temp_addr))
	  {
	    tree t = make_ssa_name (TREE_TYPE (temp_addr));
	    gimple *g = gimple_build_assign (t, temp_addr);
	    gsi_insert_before (gsi, g, GSI_SAME_STMT);
	    temp_addr = t;
	  }
	gimple *g;
	g = gimple_build_assign (build2 (MEM_REF, align_stype, temp_addr,
					 build_int_cst (arg2_type, 0)), arg0);
	gimple_set_location (g, loc);
	gsi_replace (gsi, g, true);
	return true;
      }

    /* Vector Fused multiply-add (fma).  */
    case RS6000_BIF_VMADDFP:
    case RS6000_BIF_XVMADDDP:
    case RS6000_BIF_XVMADDSP:
    case RS6000_BIF_VMLADDUHM:
      {
	arg0 = gimple_call_arg (stmt, 0);
	arg1 = gimple_call_arg (stmt, 1);
	tree arg2 = gimple_call_arg (stmt, 2);
	lhs = gimple_call_lhs (stmt);
	gcall *g = gimple_build_call_internal (IFN_FMA, 3, arg0, arg1, arg2);
	gimple_call_set_lhs (g, lhs);
	gimple_call_set_nothrow (g, true);
	gimple_set_location (g, gimple_location (stmt));
	gsi_replace (gsi, g, true);
	return true;
      }

    /* Vector compares; EQ, NE, GE, GT, LE.  */
    case RS6000_BIF_VCMPEQUB:
    case RS6000_BIF_VCMPEQUH:
    case RS6000_BIF_VCMPEQUW:
    case RS6000_BIF_VCMPEQUD:
    case RS6000_BIF_VCMPEQUT:
      fold_compare_helper (gsi, EQ_EXPR, stmt);
      return true;

    case RS6000_BIF_VCMPNEB:
    case RS6000_BIF_VCMPNEH:
    case RS6000_BIF_VCMPNEW:
    case RS6000_BIF_VCMPNET:
      fold_compare_helper (gsi, NE_EXPR, stmt);
      return true;

    case RS6000_BIF_CMPGE_16QI:
    case RS6000_BIF_CMPGE_U16QI:
    case RS6000_BIF_CMPGE_8HI:
    case RS6000_BIF_CMPGE_U8HI:
    case RS6000_BIF_CMPGE_4SI:
    case RS6000_BIF_CMPGE_U4SI:
    case RS6000_BIF_CMPGE_2DI:
    case RS6000_BIF_CMPGE_U2DI:
    case RS6000_BIF_CMPGE_1TI:
    case RS6000_BIF_CMPGE_U1TI:
      fold_compare_helper (gsi, GE_EXPR, stmt);
      return true;

    case RS6000_BIF_VCMPGTSB:
    case RS6000_BIF_VCMPGTUB:
    case RS6000_BIF_VCMPGTSH:
    case RS6000_BIF_VCMPGTUH:
    case RS6000_BIF_VCMPGTSW:
    case RS6000_BIF_VCMPGTUW:
    case RS6000_BIF_VCMPGTUD:
    case RS6000_BIF_VCMPGTSD:
    case RS6000_BIF_VCMPGTUT:
    case RS6000_BIF_VCMPGTST:
      fold_compare_helper (gsi, GT_EXPR, stmt);
      return true;

    /* flavors of vec_splat_[us]{8,16,32}.  */
    case RS6000_BIF_VSPLTISB:
    case RS6000_BIF_VSPLTISH:
    case RS6000_BIF_VSPLTISW:
      {
	arg0 = gimple_call_arg (stmt, 0);
	lhs = gimple_call_lhs (stmt);

	/* Only fold the vec_splat_*() if the lower bits of arg 0 is a
	   5-bit signed constant in range -16 to +15.  */
	if (TREE_CODE (arg0) != INTEGER_CST
	    || !IN_RANGE (TREE_INT_CST_LOW (arg0), -16, 15))
	  return false;
	gimple_seq stmts = NULL;
	location_t loc = gimple_location (stmt);
	tree splat_value = gimple_convert (&stmts, loc,
					   TREE_TYPE (TREE_TYPE (lhs)), arg0);
	gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	tree splat_tree = build_vector_from_val (TREE_TYPE (lhs), splat_value);
	g = gimple_build_assign (lhs, splat_tree);
	gimple_set_location (g, gimple_location (stmt));
	gsi_replace (gsi, g, true);
	return true;
      }

    /* Flavors of vec_splat.  */
    /* a = vec_splat (b, 0x3) becomes a = { b[3],b[3],b[3],...};  */
    case RS6000_BIF_VSPLTB:
    case RS6000_BIF_VSPLTH:
    case RS6000_BIF_VSPLTW:
    case RS6000_BIF_XXSPLTD_V2DI:
    case RS6000_BIF_XXSPLTD_V2DF:
      {
	arg0 = gimple_call_arg (stmt, 0); /* input vector.  */
	arg1 = gimple_call_arg (stmt, 1); /* index into arg0.  */
	/* Only fold the vec_splat_*() if arg1 is both a constant value and
	   is a valid index into the arg0 vector.  */
	unsigned int n_elts = VECTOR_CST_NELTS (arg0);
	if (TREE_CODE (arg1) != INTEGER_CST
	    || TREE_INT_CST_LOW (arg1) > (n_elts -1))
	  return false;
	lhs = gimple_call_lhs (stmt);
	tree lhs_type = TREE_TYPE (lhs);
	tree arg0_type = TREE_TYPE (arg0);
	tree splat;
	if (TREE_CODE (arg0) == VECTOR_CST)
	  splat = VECTOR_CST_ELT (arg0, TREE_INT_CST_LOW (arg1));
	else
	  {
	    /* Determine (in bits) the length and start location of the
	       splat value for a call to the tree_vec_extract helper.  */
	    int splat_elem_size = TREE_INT_CST_LOW (size_in_bytes (arg0_type))
				  * BITS_PER_UNIT / n_elts;
	    int splat_start_bit = TREE_INT_CST_LOW (arg1) * splat_elem_size;
	    tree len = build_int_cst (bitsizetype, splat_elem_size);
	    tree start = build_int_cst (bitsizetype, splat_start_bit);
	    splat = tree_vec_extract (gsi, TREE_TYPE (lhs_type), arg0,
				      len, start);
	  }
	/* And finally, build the new vector.  */
	tree splat_tree = build_vector_from_val (lhs_type, splat);
	g = gimple_build_assign (lhs, splat_tree);
	gimple_set_location (g, gimple_location (stmt));
	gsi_replace (gsi, g, true);
	return true;
      }

    /* vec_mergel (integrals).  */
    case RS6000_BIF_VMRGLH:
    case RS6000_BIF_VMRGLW:
    case RS6000_BIF_VMRGLB:
    case RS6000_BIF_VEC_MERGEL_V2DI:
    case RS6000_BIF_VEC_MERGEL_V2DF:
      fold_mergehl_helper (gsi, stmt, 1);
      return true;
    /* vec_mergeh (integrals).  */
    case RS6000_BIF_VMRGHH:
    case RS6000_BIF_VMRGHW:
    case RS6000_BIF_VMRGHB:
    case RS6000_BIF_VEC_MERGEH_V2DI:
    case RS6000_BIF_VEC_MERGEH_V2DF:
      fold_mergehl_helper (gsi, stmt, 0);
      return true;

    /* Flavors of vec_mergee.  */
    case RS6000_BIF_VMRGEW_V4SI:
    case RS6000_BIF_VMRGEW_V2DI:
    case RS6000_BIF_VMRGEW_V4SF:
    case RS6000_BIF_VMRGEW_V2DF:
      fold_mergeeo_helper (gsi, stmt, 0);
      return true;
    /* Flavors of vec_mergeo.  */
    case RS6000_BIF_VMRGOW_V4SI:
    case RS6000_BIF_VMRGOW_V2DI:
    case RS6000_BIF_VMRGOW_V4SF:
    case RS6000_BIF_VMRGOW_V2DF:
      fold_mergeeo_helper (gsi, stmt, 1);
      return true;

    /* d = vec_pack (a, b) */
    case RS6000_BIF_VPKUDUM:
    case RS6000_BIF_VPKUHUM:
    case RS6000_BIF_VPKUWUM:
      {
	arg0 = gimple_call_arg (stmt, 0);
	arg1 = gimple_call_arg (stmt, 1);
	lhs = gimple_call_lhs (stmt);
	gimple *g = gimple_build_assign (lhs, VEC_PACK_TRUNC_EXPR, arg0, arg1);
	gimple_set_location (g, gimple_location (stmt));
	gsi_replace (gsi, g, true);
	return true;
      }

    /* d = vec_unpackh (a) */
    /* Note that the UNPACK_{HI,LO}_EXPR used in the gimple_build_assign call
       in this code is sensitive to endian-ness, and needs to be inverted to
       handle both LE and BE targets.  */
    case RS6000_BIF_VUPKHSB:
    case RS6000_BIF_VUPKHSH:
    case RS6000_BIF_VUPKHSW:
      {
	arg0 = gimple_call_arg (stmt, 0);
	lhs = gimple_call_lhs (stmt);
	if (BYTES_BIG_ENDIAN)
	  g = gimple_build_assign (lhs, VEC_UNPACK_HI_EXPR, arg0);
	else
	  g = gimple_build_assign (lhs, VEC_UNPACK_LO_EXPR, arg0);
	gimple_set_location (g, gimple_location (stmt));
	gsi_replace (gsi, g, true);
	return true;
      }
    /* d = vec_unpackl (a) */
    case RS6000_BIF_VUPKLSB:
    case RS6000_BIF_VUPKLSH:
    case RS6000_BIF_VUPKLSW:
      {
	arg0 = gimple_call_arg (stmt, 0);
	lhs = gimple_call_lhs (stmt);
	if (BYTES_BIG_ENDIAN)
	  g = gimple_build_assign (lhs, VEC_UNPACK_LO_EXPR, arg0);
	else
	  g = gimple_build_assign (lhs, VEC_UNPACK_HI_EXPR, arg0);
	gimple_set_location (g, gimple_location (stmt));
	gsi_replace (gsi, g, true);
	return true;
      }
    /* There is no gimple type corresponding with pixel, so just return.  */
    case RS6000_BIF_VUPKHPX:
    case RS6000_BIF_VUPKLPX:
      return false;

    /* vec_perm.  */
    case RS6000_BIF_VPERM_16QI:
    case RS6000_BIF_VPERM_8HI:
    case RS6000_BIF_VPERM_4SI:
    case RS6000_BIF_VPERM_2DI:
    case RS6000_BIF_VPERM_4SF:
    case RS6000_BIF_VPERM_2DF:
    case RS6000_BIF_VPERM_16QI_UNS:
    case RS6000_BIF_VPERM_8HI_UNS:
    case RS6000_BIF_VPERM_4SI_UNS:
    case RS6000_BIF_VPERM_2DI_UNS:
      {
	arg0 = gimple_call_arg (stmt, 0);
	arg1 = gimple_call_arg (stmt, 1);
	tree permute = gimple_call_arg (stmt, 2);
	lhs = gimple_call_lhs (stmt);
	location_t loc = gimple_location (stmt);
	gimple_seq stmts = NULL;
	// convert arg0 and arg1 to match the type of the permute
	// for the VEC_PERM_EXPR operation.
	tree permute_type = (TREE_TYPE (permute));
	tree arg0_ptype = gimple_build (&stmts, loc, VIEW_CONVERT_EXPR,
					permute_type, arg0);
	tree arg1_ptype = gimple_build (&stmts, loc, VIEW_CONVERT_EXPR,
					permute_type, arg1);
	tree lhs_ptype = gimple_build (&stmts, loc, VEC_PERM_EXPR,
				      permute_type, arg0_ptype, arg1_ptype,
				      permute);
	// Convert the result back to the desired lhs type upon completion.
	tree temp = gimple_build (&stmts, loc, VIEW_CONVERT_EXPR,
				  TREE_TYPE (lhs), lhs_ptype);
	gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	g = gimple_build_assign (lhs, temp);
	gimple_set_location (g, loc);
	gsi_replace (gsi, g, true);
	return true;
      }

    default:
      if (TARGET_DEBUG_BUILTIN)
	fprintf (stderr, "gimple builtin intrinsic not matched:%d %s %s\n",
		 fn_code, fn_name1, fn_name2);
      break;
    }

  return false;
}

/* **** Expansion support ****  */

static rtx
altivec_expand_predicate_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat, scratch;
  tree cr6_form = CALL_EXPR_ARG (exp, 0);
  tree arg0 = CALL_EXPR_ARG (exp, 1);
  tree arg1 = CALL_EXPR_ARG (exp, 2);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  machine_mode tmode = SImode;
  machine_mode mode0 = insn_data[icode].operand[1].mode;
  machine_mode mode1 = insn_data[icode].operand[2].mode;
  int cr6_form_int;

  if (TREE_CODE (cr6_form) != INTEGER_CST)
    {
      error ("argument 1 of %qs must be a constant",
	     "__builtin_altivec_predicate");
      return const0_rtx;
    }
  else
    cr6_form_int = TREE_INT_CST_LOW (cr6_form);

  gcc_assert (mode0 == mode1);

  /* If we have invalid arguments, bail out before generating bad rtl.  */
  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return const0_rtx;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  /* Note that for many of the relevant operations (e.g. cmpne or
     cmpeq) with float or double operands, it makes more sense for the
     mode of the allocated scratch register to select a vector of
     integer.  But the choice to copy the mode of operand 0 was made
     long ago and there are no plans to change it.  */
  scratch = gen_reg_rtx (mode0);

  pat = GEN_FCN (icode) (scratch, op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);

  /* The vec_any* and vec_all* predicates use the same opcodes for two
     different operations, but the bits in CR6 will be different
     depending on what information we want.  So we have to play tricks
     with CR6 to get the right bits out.

     If you think this is disgusting, look at the specs for the
     AltiVec predicates.  */

  switch (cr6_form_int)
    {
    case 0:
      emit_insn (gen_cr6_test_for_zero (target));
      break;
    case 1:
      emit_insn (gen_cr6_test_for_zero_reverse (target));
      break;
    case 2:
      emit_insn (gen_cr6_test_for_lt (target));
      break;
    case 3:
      emit_insn (gen_cr6_test_for_lt_reverse (target));
      break;
    default:
      error ("argument 1 of %qs is out of range",
	     "__builtin_altivec_predicate");
      break;
    }

  return target;
}

/* Expand vec_ext builtin.  */
static rtx
altivec_expand_vec_ext_builtin (tree exp, rtx target)
{
  machine_mode tmode, mode0;
  tree arg0, arg1;
  rtx op0;
  rtx op1;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);

  op0 = expand_normal (arg0);
  op1 = expand_normal (arg1);

  if (TREE_CODE (arg1) == INTEGER_CST)
    {
      unsigned HOST_WIDE_INT elt;
      unsigned HOST_WIDE_INT size = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));
      unsigned int truncated_selector;
      /* Even if !tree_fits_uhwi_p (arg1)), TREE_INT_CST_LOW (arg0)
	 returns low-order bits of INTEGER_CST for modulo indexing.  */
      elt = TREE_INT_CST_LOW (arg1);
      truncated_selector = elt % size;
      op1 = GEN_INT (truncated_selector);
    }

  tmode = TYPE_MODE (TREE_TYPE (TREE_TYPE (arg0)));
  mode0 = TYPE_MODE (TREE_TYPE (arg0));
  gcc_assert (VECTOR_MODE_P (mode0));

  op0 = force_reg (mode0, op0);

  if (optimize || !target || !register_operand (target, tmode))
    target = gen_reg_rtx (tmode);

  rs6000_expand_vector_extract (target, op0, op1);

  return target;
}

/* Expand ALTIVEC_BUILTIN_MASK_FOR_LOAD.  */
rtx
rs6000_expand_ldst_mask (rtx target, tree arg0)
{
  int icode2 = BYTES_BIG_ENDIAN ? (int) CODE_FOR_altivec_lvsr_direct
				: (int) CODE_FOR_altivec_lvsl_direct;
  machine_mode tmode = insn_data[icode2].operand[0].mode;
  machine_mode mode = insn_data[icode2].operand[1].mode;

  gcc_assert (TARGET_ALTIVEC);

  gcc_assert (POINTER_TYPE_P (TREE_TYPE (arg0)));
  rtx op = expand_expr (arg0, NULL_RTX, Pmode, EXPAND_NORMAL);
  rtx addr = memory_address (mode, op);
  /* We need to negate the address.  */
  op = gen_reg_rtx (GET_MODE (addr));
  emit_insn (gen_rtx_SET (op, gen_rtx_NEG (GET_MODE (addr), addr)));
  op = gen_rtx_MEM (mode, op);

  if (target == 0
      || GET_MODE (target) != tmode
      || !insn_data[icode2].operand[0].predicate (target, tmode))
    target = gen_reg_rtx (tmode);

  rtx pat = GEN_FCN (icode2) (target, op);
  if (!pat)
    return 0;
  emit_insn (pat);

  return target;
}

/* Used by __builtin_cpu_is(), mapping from PLATFORM names to values.  */
static const struct
{
  const char *cpu;
  unsigned int cpuid;
} cpu_is_info[] = {
  { "power11",	   PPC_PLATFORM_POWER11 },
  { "power10",	   PPC_PLATFORM_POWER10 },
  { "power9",	   PPC_PLATFORM_POWER9 },
  { "power8",	   PPC_PLATFORM_POWER8 },
  { "power7",	   PPC_PLATFORM_POWER7 },
  { "power6x",	   PPC_PLATFORM_POWER6X },
  { "power6",	   PPC_PLATFORM_POWER6 },
  { "power5+",	   PPC_PLATFORM_POWER5_PLUS },
  { "power5",	   PPC_PLATFORM_POWER5 },
  { "ppc970",	   PPC_PLATFORM_PPC970 },
  { "power4",	   PPC_PLATFORM_POWER4 },
  { "ppca2",	   PPC_PLATFORM_PPCA2 },
  { "ppc476",	   PPC_PLATFORM_PPC476 },
  { "ppc464",	   PPC_PLATFORM_PPC464 },
  { "ppc440",	   PPC_PLATFORM_PPC440 },
  { "ppc405",	   PPC_PLATFORM_PPC405 },
  { "ppc-cell-be", PPC_PLATFORM_CELL_BE }
};

/* Used by __builtin_cpu_supports(), mapping from HWCAP names to masks.  */
static const struct
{
  const char *hwcap;
  int mask;
  unsigned int id;
} cpu_supports_info[] = {
  /* AT_HWCAP masks.  */
  { "4xxmac",		PPC_FEATURE_HAS_4xxMAC,		0 },
  { "altivec",		PPC_FEATURE_HAS_ALTIVEC,	0 },
  { "arch_2_05",	PPC_FEATURE_ARCH_2_05,		0 },
  { "arch_2_06",	PPC_FEATURE_ARCH_2_06,		0 },
  { "archpmu",		PPC_FEATURE_PERFMON_COMPAT,	0 },
  { "booke",		PPC_FEATURE_BOOKE,		0 },
  { "cellbe",		PPC_FEATURE_CELL_BE,		0 },
  { "dfp",		PPC_FEATURE_HAS_DFP,		0 },
  { "efpdouble",	PPC_FEATURE_HAS_EFP_DOUBLE,	0 },
  { "efpsingle",	PPC_FEATURE_HAS_EFP_SINGLE,	0 },
  { "fpu",		PPC_FEATURE_HAS_FPU,		0 },
  { "ic_snoop",		PPC_FEATURE_ICACHE_SNOOP,	0 },
  { "mmu",		PPC_FEATURE_HAS_MMU,		0 },
  { "notb",		PPC_FEATURE_NO_TB,		0 },
  { "pa6t",		PPC_FEATURE_PA6T,		0 },
  { "power4",		PPC_FEATURE_POWER4,		0 },
  { "power5",		PPC_FEATURE_POWER5,		0 },
  { "power5+",		PPC_FEATURE_POWER5_PLUS,	0 },
  { "power6x",		PPC_FEATURE_POWER6_EXT,		0 },
  { "ppc32",		PPC_FEATURE_32,			0 },
  { "ppc601",		PPC_FEATURE_601_INSTR,		0 },
  { "ppc64",		PPC_FEATURE_64,			0 },
  { "ppcle",		PPC_FEATURE_PPC_LE,		0 },
  { "smt",		PPC_FEATURE_SMT,		0 },
  { "spe",		PPC_FEATURE_HAS_SPE,		0 },
  { "true_le",		PPC_FEATURE_TRUE_LE,		0 },
  { "ucache",		PPC_FEATURE_UNIFIED_CACHE,	0 },
  { "vsx",		PPC_FEATURE_HAS_VSX,		0 },

  /* AT_HWCAP2 masks.  */
  { "arch_2_07",	PPC_FEATURE2_ARCH_2_07,		1 },
  { "dscr",		PPC_FEATURE2_HAS_DSCR,		1 },
  { "ebb",		PPC_FEATURE2_HAS_EBB,		1 },
  { "htm",		PPC_FEATURE2_HAS_HTM,		1 },
  { "htm-nosc",		PPC_FEATURE2_HTM_NOSC,		1 },
  { "htm-no-suspend",	PPC_FEATURE2_HTM_NO_SUSPEND,	1 },
  { "isel",		PPC_FEATURE2_HAS_ISEL,		1 },
  { "tar",		PPC_FEATURE2_HAS_TAR,		1 },
  { "vcrypto",		PPC_FEATURE2_HAS_VEC_CRYPTO,	1 },
  { "arch_3_00",	PPC_FEATURE2_ARCH_3_00,		1 },
  { "ieee128",		PPC_FEATURE2_HAS_IEEE128,	1 },
  { "darn",		PPC_FEATURE2_DARN,		1 },
  { "scv",		PPC_FEATURE2_SCV,		1 },
  { "arch_3_1",		PPC_FEATURE2_ARCH_3_1,		1 },
  { "mma",		PPC_FEATURE2_MMA,		1 },
};

/* Expand the CPU builtin in FCODE and store the result in TARGET.  */
static rtx
cpu_expand_builtin (enum rs6000_gen_builtins fcode,
		    tree exp ATTRIBUTE_UNUSED, rtx target)
{
  /* __builtin_cpu_init () is a nop, so expand to nothing.  */
  if (fcode == RS6000_BIF_CPU_INIT)
    return const0_rtx;

  if (target == 0 || GET_MODE (target) != SImode)
    target = gen_reg_rtx (SImode);

  /* TODO: Factor the #ifdef'd code into a separate function.  */
#ifdef TARGET_LIBC_PROVIDES_HWCAP_IN_TCB
  tree arg = TREE_OPERAND (CALL_EXPR_ARG (exp, 0), 0);
  /* Target clones creates an ARRAY_REF instead of STRING_CST, convert it back
     to a STRING_CST.  */
  if (TREE_CODE (arg) == ARRAY_REF
      && TREE_CODE (TREE_OPERAND (arg, 0)) == STRING_CST
      && TREE_CODE (TREE_OPERAND (arg, 1)) == INTEGER_CST
      && compare_tree_int (TREE_OPERAND (arg, 1), 0) == 0)
    arg = TREE_OPERAND (arg, 0);

  if (TREE_CODE (arg) != STRING_CST)
    {
      error ("builtin %qs only accepts a string argument",
	     rs6000_builtin_info[(size_t) fcode].bifname);
      return const0_rtx;
    }

  if (fcode == RS6000_BIF_CPU_IS)
    {
      const char *cpu = TREE_STRING_POINTER (arg);
      rtx cpuid = NULL_RTX;
      for (size_t i = 0; i < ARRAY_SIZE (cpu_is_info); i++)
	if (strcmp (cpu, cpu_is_info[i].cpu) == 0)
	  {
	    /* The CPUID value in the TCB is offset by _DL_FIRST_PLATFORM.  */
	    cpuid = GEN_INT (cpu_is_info[i].cpuid + _DL_FIRST_PLATFORM);
	    break;
	  }
      if (cpuid == NULL_RTX)
	{
	  /* Invalid CPU argument.  */
	  error ("cpu %qs is an invalid argument to builtin %qs",
		 cpu, rs6000_builtin_info[(size_t) fcode].bifname);
	  return const0_rtx;
	}

      rtx platform = gen_reg_rtx (SImode);
      rtx address = gen_rtx_PLUS (Pmode,
				  gen_rtx_REG (Pmode, TLS_REGNUM),
				  GEN_INT (TCB_PLATFORM_OFFSET));
      rtx tcbmem = gen_const_mem (SImode, address);
      emit_move_insn (platform, tcbmem);
      emit_insn (gen_eqsi3 (target, platform, cpuid));
    }
  else if (fcode == RS6000_BIF_CPU_SUPPORTS)
    {
      const char *hwcap = TREE_STRING_POINTER (arg);
      rtx mask = NULL_RTX;
      int hwcap_offset;
      for (size_t i = 0; i < ARRAY_SIZE (cpu_supports_info); i++)
	if (strcmp (hwcap, cpu_supports_info[i].hwcap) == 0)
	  {
	    mask = GEN_INT (cpu_supports_info[i].mask);
	    hwcap_offset = TCB_HWCAP_OFFSET (cpu_supports_info[i].id);
	    break;
	  }
      if (mask == NULL_RTX)
	{
	  /* Invalid HWCAP argument.  */
	  error ("%s %qs is an invalid argument to builtin %qs",
		 "hwcap", hwcap,
		 rs6000_builtin_info[(size_t) fcode].bifname);
	  return const0_rtx;
	}

      rtx tcb_hwcap = gen_reg_rtx (SImode);
      rtx address = gen_rtx_PLUS (Pmode,
				  gen_rtx_REG (Pmode, TLS_REGNUM),
				  GEN_INT (hwcap_offset));
      rtx tcbmem = gen_const_mem (SImode, address);
      emit_move_insn (tcb_hwcap, tcbmem);
      rtx scratch1 = gen_reg_rtx (SImode);
      emit_insn (gen_rtx_SET (scratch1,
			      gen_rtx_AND (SImode, tcb_hwcap, mask)));
      rtx scratch2 = gen_reg_rtx (SImode);
      emit_insn (gen_eqsi3 (scratch2, scratch1, const0_rtx));
      emit_insn (gen_rtx_SET (target,
			      gen_rtx_XOR (SImode, scratch2, const1_rtx)));
    }
  else
    gcc_unreachable ();

  /* Record that we have expanded a CPU builtin, so that we can later
     emit a reference to the special symbol exported by LIBC to ensure we
     do not link against an old LIBC that doesn't support this feature.  */
  cpu_builtin_p = true;

#else
  warning (0, "builtin %qs needs GLIBC (2.23 and newer) that exports hardware "
	   "capability bits", rs6000_builtin_info[(size_t) fcode].bifname);

  /* For old LIBCs, always return FALSE.  */
  emit_move_insn (target, GEN_INT (0));
#endif /* TARGET_LIBC_PROVIDES_HWCAP_IN_TCB */

  return target;
}

/* For the element-reversing load/store built-ins, produce the correct
   insn_code depending on the target endianness.  */
static insn_code
elemrev_icode (rs6000_gen_builtins fcode)
{
  switch (fcode)
    {
    case RS6000_BIF_ST_ELEMREV_V1TI:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_store_v1ti
			      : CODE_FOR_vsx_st_elemrev_v1ti;

    case RS6000_BIF_ST_ELEMREV_V2DF:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_store_v2df
			      : CODE_FOR_vsx_st_elemrev_v2df;

    case RS6000_BIF_ST_ELEMREV_V2DI:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_store_v2di
			      : CODE_FOR_vsx_st_elemrev_v2di;

    case RS6000_BIF_ST_ELEMREV_V4SF:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_store_v4sf
			      : CODE_FOR_vsx_st_elemrev_v4sf;

    case RS6000_BIF_ST_ELEMREV_V4SI:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_store_v4si
			      : CODE_FOR_vsx_st_elemrev_v4si;

    case RS6000_BIF_ST_ELEMREV_V8HI:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_store_v8hi
			      : CODE_FOR_vsx_st_elemrev_v8hi;

    case RS6000_BIF_ST_ELEMREV_V16QI:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_store_v16qi
			      : CODE_FOR_vsx_st_elemrev_v16qi;

    case RS6000_BIF_LD_ELEMREV_V2DF:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_load_v2df
			      : CODE_FOR_vsx_ld_elemrev_v2df;

    case RS6000_BIF_LD_ELEMREV_V1TI:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_load_v1ti
			      : CODE_FOR_vsx_ld_elemrev_v1ti;

    case RS6000_BIF_LD_ELEMREV_V2DI:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_load_v2di
			      : CODE_FOR_vsx_ld_elemrev_v2di;

    case RS6000_BIF_LD_ELEMREV_V4SF:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_load_v4sf
			      : CODE_FOR_vsx_ld_elemrev_v4sf;

    case RS6000_BIF_LD_ELEMREV_V4SI:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_load_v4si
			      : CODE_FOR_vsx_ld_elemrev_v4si;

    case RS6000_BIF_LD_ELEMREV_V8HI:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_load_v8hi
			      : CODE_FOR_vsx_ld_elemrev_v8hi;

    case RS6000_BIF_LD_ELEMREV_V16QI:
      return BYTES_BIG_ENDIAN ? CODE_FOR_vsx_load_v16qi
			      : CODE_FOR_vsx_ld_elemrev_v16qi;
    default:
      ;
    }

  gcc_unreachable ();
}

/* Expand an AltiVec vector load builtin, and return the expanded rtx.  */
static rtx
ldv_expand_builtin (rtx target, insn_code icode, rtx *op, machine_mode tmode)
{
  if (target == 0
      || GET_MODE (target) != tmode
      || !insn_data[icode].operand[0].predicate (target, tmode))
    target = gen_reg_rtx (tmode);

  op[1] = copy_to_mode_reg (Pmode, op[1]);

  /* These CELL built-ins use BLKmode instead of tmode for historical
     (i.e., unknown) reasons.  TODO: Is this necessary?  */
  bool blk = (icode == CODE_FOR_altivec_lvlx
	      || icode == CODE_FOR_altivec_lvlxl
	      || icode == CODE_FOR_altivec_lvrx
	      || icode == CODE_FOR_altivec_lvrxl);

  /* For LVX, express the RTL accurately by ANDing the address with -16.
     LVXL and LVE*X expand to use UNSPECs to hide their special behavior,
     so the raw address is fine.  */
  /* TODO: That statement seems wrong, as the UNSPECs don't surround the
     memory expression, so a latent bug may lie here.  The &-16 is likely
     needed for all VMX-style loads.  */
  if (icode == CODE_FOR_altivec_lvx_v1ti
      || icode == CODE_FOR_altivec_lvx_v2df
      || icode == CODE_FOR_altivec_lvx_v2di
      || icode == CODE_FOR_altivec_lvx_v4sf
      || icode == CODE_FOR_altivec_lvx_v4si
      || icode == CODE_FOR_altivec_lvx_v8hi
      || icode == CODE_FOR_altivec_lvx_v16qi)
    {
      rtx rawaddr;
      if (op[0] == const0_rtx)
	rawaddr = op[1];
      else
	{
	  op[0] = copy_to_mode_reg (Pmode, op[0]);
	  rawaddr = gen_rtx_PLUS (Pmode, op[1], op[0]);
	}
      rtx addr = gen_rtx_AND (Pmode, rawaddr, gen_rtx_CONST_INT (Pmode, -16));
      addr = gen_rtx_MEM (blk ? BLKmode : tmode, addr);

      emit_insn (gen_rtx_SET (target, addr));
    }
  else
    {
      rtx addr;
      if (op[0] == const0_rtx)
	addr = gen_rtx_MEM (blk ? BLKmode : tmode, op[1]);
      else
	{
	  op[0] = copy_to_mode_reg (Pmode, op[0]);
	  addr = gen_rtx_MEM (blk ? BLKmode : tmode,
			      gen_rtx_PLUS (Pmode, op[1], op[0]));
	}

      rtx pat = GEN_FCN (icode) (target, addr);
      if (!pat)
	return 0;
      emit_insn (pat);
    }

  return target;
}

/* Expand a builtin function that loads a scalar into a vector register
   with sign extension, and return the expanded rtx.  */
static rtx
lxvrse_expand_builtin (rtx target, insn_code icode, rtx *op,
		       machine_mode tmode, machine_mode smode)
{
  rtx pat, addr;
  op[1] = copy_to_mode_reg (Pmode, op[1]);

  if (op[0] == const0_rtx)
    addr = gen_rtx_MEM (tmode, op[1]);
  else
    {
      op[0] = copy_to_mode_reg (Pmode, op[0]);
      addr = gen_rtx_MEM (smode,
			  gen_rtx_PLUS (Pmode, op[1], op[0]));
    }

  rtx discratch = gen_reg_rtx (V2DImode);
  rtx tiscratch = gen_reg_rtx (TImode);

  /* Emit the lxvr*x insn.  */
  pat = GEN_FCN (icode) (tiscratch, addr);
  if (!pat)
    return 0;
  emit_insn (pat);

  /* Emit a sign extension from V16QI,V8HI,V4SI to V2DI.  */
  rtx temp1;
  if (icode == CODE_FOR_vsx_lxvrbx)
    {
      temp1  = simplify_gen_subreg (V16QImode, tiscratch, TImode, 0);
      emit_insn (gen_vsx_sign_extend_v16qi_v2di (discratch, temp1));
    }
  else if (icode == CODE_FOR_vsx_lxvrhx)
    {
      temp1  = simplify_gen_subreg (V8HImode, tiscratch, TImode, 0);
      emit_insn (gen_vsx_sign_extend_v8hi_v2di (discratch, temp1));
    }
  else if (icode == CODE_FOR_vsx_lxvrwx)
    {
      temp1  = simplify_gen_subreg (V4SImode, tiscratch, TImode, 0);
      emit_insn (gen_vsx_sign_extend_v4si_v2di (discratch, temp1));
    }
  else if (icode == CODE_FOR_vsx_lxvrdx)
    discratch = simplify_gen_subreg (V2DImode, tiscratch, TImode, 0);
  else
    gcc_unreachable ();

  /* Emit the sign extension from V2DI (double) to TI (quad).  */
  rtx temp2 = simplify_gen_subreg (TImode, discratch, V2DImode, 0);
  emit_insn (gen_extendditi2_vector (target, temp2));

  return target;
}

/* Expand a builtin function that loads a scalar into a vector register
   with zero extension, and return the expanded rtx.  */
static rtx
lxvrze_expand_builtin (rtx target, insn_code icode, rtx *op,
		       machine_mode tmode, machine_mode smode)
{
  rtx pat, addr;
  op[1] = copy_to_mode_reg (Pmode, op[1]);

  if (op[0] == const0_rtx)
    addr = gen_rtx_MEM (tmode, op[1]);
  else
    {
      op[0] = copy_to_mode_reg (Pmode, op[0]);
      addr = gen_rtx_MEM (smode,
			  gen_rtx_PLUS (Pmode, op[1], op[0]));
    }

  pat = GEN_FCN (icode) (target, addr);
  if (!pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Expand an AltiVec vector store builtin, and return the expanded rtx.  */
static rtx
stv_expand_builtin (insn_code icode, rtx *op,
		    machine_mode tmode, machine_mode smode)
{
  op[2] = copy_to_mode_reg (Pmode, op[2]);

  /* For STVX, express the RTL accurately by ANDing the address with -16.
     STVXL and STVE*X expand to use UNSPECs to hide their special behavior,
     so the raw address is fine.  */
  /* TODO: That statement seems wrong, as the UNSPECs don't surround the
     memory expression, so a latent bug may lie here.  The &-16 is likely
     needed for all VMX-style stores.  */
  if (icode == CODE_FOR_altivec_stvx_v2df
      || icode == CODE_FOR_altivec_stvx_v2di
      || icode == CODE_FOR_altivec_stvx_v4sf
      || icode == CODE_FOR_altivec_stvx_v4si
      || icode == CODE_FOR_altivec_stvx_v8hi
      || icode == CODE_FOR_altivec_stvx_v16qi)
    {
      rtx rawaddr;
      if (op[1] == const0_rtx)
	rawaddr = op[2];
      else
	{
	  op[1] = copy_to_mode_reg (Pmode, op[1]);
	  rawaddr = gen_rtx_PLUS (Pmode, op[2], op[1]);
	}

      rtx addr = gen_rtx_AND (Pmode, rawaddr, gen_rtx_CONST_INT (Pmode, -16));
      addr = gen_rtx_MEM (tmode, addr);
      op[0] = copy_to_mode_reg (tmode, op[0]);
      emit_insn (gen_rtx_SET (addr, op[0]));
    }
  else if (icode == CODE_FOR_vsx_stxvrbx
	   || icode == CODE_FOR_vsx_stxvrhx
	   || icode == CODE_FOR_vsx_stxvrwx
	   || icode == CODE_FOR_vsx_stxvrdx)
    {
      rtx truncrtx = gen_rtx_TRUNCATE (tmode, op[0]);
      op[0] = copy_to_mode_reg (E_TImode, truncrtx);

      rtx addr;
      if (op[1] == const0_rtx)
	addr = gen_rtx_MEM (tmode, op[2]);
      else
	{
	  op[1] = copy_to_mode_reg (Pmode, op[1]);
	  addr = gen_rtx_MEM (tmode, gen_rtx_PLUS (Pmode, op[2], op[1]));
	}
      rtx pat = GEN_FCN (icode) (addr, op[0]);
      if (pat)
	emit_insn (pat);
    }
  else
    {
      if (!insn_data[icode].operand[1].predicate (op[0], smode))
	op[0] = copy_to_mode_reg (smode, op[0]);

      rtx addr;
      if (op[1] == const0_rtx)
	addr = gen_rtx_MEM (tmode, op[2]);
      else
	{
	  op[1] = copy_to_mode_reg (Pmode, op[1]);
	  addr = gen_rtx_MEM (tmode, gen_rtx_PLUS (Pmode, op[2], op[1]));
	}

      rtx pat = GEN_FCN (icode) (addr, op[0]);
      if (pat)
	emit_insn (pat);
    }

  return NULL_RTX;
}

/* Expand the MMA built-in in EXP, and return it.  */
static rtx
mma_expand_builtin (tree exp, rtx target, insn_code icode,
		    rs6000_gen_builtins fcode)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  bool void_func = TREE_TYPE (TREE_TYPE (fndecl)) == void_type_node;
  machine_mode tmode = VOIDmode;
  rtx op[MAX_MMA_OPERANDS];
  unsigned nopnds = 0;

  if (!void_func)
    {
      tmode = insn_data[icode].operand[0].mode;
      if (!(target
	    && GET_MODE (target) == tmode
	    && insn_data[icode].operand[0].predicate (target, tmode)))
	target = gen_reg_rtx (tmode);
      op[nopnds++] = target;
    }
  else
    target = const0_rtx;

  call_expr_arg_iterator iter;
  tree arg;
  FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
    {
      if (arg == error_mark_node)
	return const0_rtx;

      rtx opnd;
      const struct insn_operand_data *insn_op;
      insn_op = &insn_data[icode].operand[nopnds];
      if (TREE_CODE (arg) == ADDR_EXPR
	  && MEM_P (DECL_RTL (TREE_OPERAND (arg, 0))))
	opnd = DECL_RTL (TREE_OPERAND (arg, 0));
      else
	opnd = expand_normal (arg);

      if (!insn_op->predicate (opnd, insn_op->mode))
	{
	  /* TODO: This use of constraints needs explanation.  */
	  if (!strcmp (insn_op->constraint, "n"))
	    {
	      if (!CONST_INT_P (opnd))
		error ("argument %d must be an unsigned literal", nopnds);
	      else
		error ("argument %d is an unsigned literal that is "
		       "out of range", nopnds);
	      return const0_rtx;
	    }
	  opnd = copy_to_mode_reg (insn_op->mode, opnd);
	}

      /* Some MMA instructions have INOUT accumulator operands, so force
	 their target register to be the same as their input register.  */
      if (!void_func
	  && nopnds == 1
	  && !strcmp (insn_op->constraint, "0")
	  && insn_op->mode == tmode
	  && REG_P (opnd)
	  && insn_data[icode].operand[0].predicate (opnd, tmode))
	target = op[0] = opnd;

      op[nopnds++] = opnd;
    }

  rtx pat;
  switch (nopnds)
    {
    case 1:
      pat = GEN_FCN (icode) (op[0]);
      break;
    case 2:
      pat = GEN_FCN (icode) (op[0], op[1]);
      break;
    case 3:
      /* The ASSEMBLE builtin source operands are reversed in little-endian
	 mode, so reorder them.  */
      if (fcode == RS6000_BIF_ASSEMBLE_PAIR_V_INTERNAL && !WORDS_BIG_ENDIAN)
	std::swap (op[1], op[2]);
      pat = GEN_FCN (icode) (op[0], op[1], op[2]);
      break;
    case 4:
      pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3]);
      break;
    case 5:
      /* The ASSEMBLE builtin source operands are reversed in little-endian
	 mode, so reorder them.  */
      if (fcode == RS6000_BIF_ASSEMBLE_ACC_INTERNAL && !WORDS_BIG_ENDIAN)
	{
	  std::swap (op[1], op[4]);
	  std::swap (op[2], op[3]);
	}
      pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3], op[4]);
      break;
    case 6:
      pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3], op[4], op[5]);
      break;
    case 7:
      pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3], op[4], op[5], op[6]);
      break;
    default:
      gcc_unreachable ();
    }

  if (!pat)
    return NULL_RTX;

  emit_insn (pat);
  return target;
}

/* Return the correct ICODE value depending on whether we are
   setting or reading the HTM SPRs.  */
static inline enum insn_code
rs6000_htm_spr_icode (bool nonvoid)
{
  if (nonvoid)
    return (TARGET_POWERPC64) ? CODE_FOR_htm_mfspr_di : CODE_FOR_htm_mfspr_si;
  else
    return (TARGET_POWERPC64) ? CODE_FOR_htm_mtspr_di : CODE_FOR_htm_mtspr_si;
}

/* Return the appropriate SPR number associated with the given builtin.  */
static inline HOST_WIDE_INT
htm_spr_num (enum rs6000_gen_builtins code)
{
  if (code == RS6000_BIF_GET_TFHAR
      || code == RS6000_BIF_SET_TFHAR)
    return TFHAR_SPR;
  else if (code == RS6000_BIF_GET_TFIAR
	   || code == RS6000_BIF_SET_TFIAR)
    return TFIAR_SPR;
  else if (code == RS6000_BIF_GET_TEXASR
	   || code == RS6000_BIF_SET_TEXASR)
    return TEXASR_SPR;
  gcc_assert (code == RS6000_BIF_GET_TEXASRU
	      || code == RS6000_BIF_SET_TEXASRU);
  return TEXASRU_SPR;
}

/* Expand the HTM builtin in EXP and store the result in TARGET.
   Return the expanded rtx.  */
static rtx
htm_expand_builtin (bifdata *bifaddr, rs6000_gen_builtins fcode,
		    tree exp, rtx target)
{
  if (!TARGET_POWERPC64
      && (fcode == RS6000_BIF_TABORTDC
	  || fcode == RS6000_BIF_TABORTDCI))
    {
      error ("builtin %qs is only valid in 64-bit mode", bifaddr->bifname);
      return const0_rtx;
    }

  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  bool nonvoid = TREE_TYPE (TREE_TYPE (fndecl)) != void_type_node;
  bool uses_spr = bif_is_htmspr (*bifaddr);
  insn_code icode = bifaddr->icode;

  if (uses_spr)
    icode = rs6000_htm_spr_icode (nonvoid);

  rtx op[MAX_HTM_OPERANDS];
  int nopnds = 0;
  const insn_operand_data *insn_op = &insn_data[icode].operand[0];

  if (nonvoid)
    {
      machine_mode tmode = (uses_spr) ? insn_op->mode : E_SImode;
      if (!target
	  || GET_MODE (target) != tmode
	  || (uses_spr && !insn_op->predicate (target, tmode)))
	target = gen_reg_rtx (tmode);
      if (uses_spr)
	op[nopnds++] = target;
    }

  tree arg;
  call_expr_arg_iterator iter;

  FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
    {
      if (arg == error_mark_node || nopnds >= MAX_HTM_OPERANDS)
	return const0_rtx;

      insn_op = &insn_data[icode].operand[nopnds];
      op[nopnds] = expand_normal (arg);

      if (!insn_op->predicate (op[nopnds], insn_op->mode))
	{
	  /* TODO: This use of constraints could use explanation.
	     This happens a couple of places, perhaps make that a
	     function to document what's happening.  */
	  if (!strcmp (insn_op->constraint, "n"))
	    {
	      int arg_num = nonvoid ? nopnds : nopnds + 1;
	      if (!CONST_INT_P (op[nopnds]))
		error ("argument %d must be an unsigned literal", arg_num);
	      else
		error ("argument %d is an unsigned literal that is "
		       "out of range", arg_num);
	      return const0_rtx;
	    }
	  op[nopnds] = copy_to_mode_reg (insn_op->mode, op[nopnds]);
	}

      nopnds++;
    }

  /* Handle the builtins for extended mnemonics.  These accept
     no arguments, but map to builtins that take arguments.  */
  switch (fcode)
    {
    case RS6000_BIF_TENDALL:  /* Alias for: tend. 1  */
    case RS6000_BIF_TRESUME:  /* Alias for: tsr. 1  */
      op[nopnds++] = GEN_INT (1);
      break;
    case RS6000_BIF_TSUSPEND: /* Alias for: tsr. 0  */
      op[nopnds++] = GEN_INT (0);
      break;
    default:
      break;
    }

  /* If this builtin accesses SPRs, then pass in the appropriate
     SPR number and SPR regno as the last two operands.  */
  rtx cr = NULL_RTX;
  if (uses_spr)
    {
      machine_mode mode = TARGET_POWERPC64 ? DImode : SImode;
      op[nopnds++] = gen_rtx_CONST_INT (mode, htm_spr_num (fcode));
    }
  /* If this builtin accesses a CR field, then pass in a scratch
     CR field as the last operand.  */
  else if (bif_is_htmcr (*bifaddr))
    {
      cr = gen_reg_rtx (CCmode);
      op[nopnds++] = cr;
    }

  rtx pat;
  switch (nopnds)
    {
    case 1:
      pat = GEN_FCN (icode) (op[0]);
      break;
    case 2:
      pat = GEN_FCN (icode) (op[0], op[1]);
      break;
    case 3:
      pat = GEN_FCN (icode) (op[0], op[1], op[2]);
      break;
    case 4:
      pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3]);
      break;
    default:
      gcc_unreachable ();
    }
  if (!pat)
    return NULL_RTX;
  emit_insn (pat);

  if (bif_is_htmcr (*bifaddr))
    {
      if (fcode == RS6000_BIF_TBEGIN)
	{
	  /* Emit code to set TARGET to true or false depending on
	     whether the tbegin. instruction succeeded or failed
	     to start a transaction.  We do this by placing the 1's
	     complement of CR's EQ bit into TARGET.  */
	  rtx scratch = gen_reg_rtx (SImode);
	  emit_insn (gen_rtx_SET (scratch,
				  gen_rtx_EQ (SImode, cr,
					      const0_rtx)));
	  emit_insn (gen_rtx_SET (target,
				  gen_rtx_XOR (SImode, scratch,
					       GEN_INT (1))));
	}
      else
	{
	  /* Emit code to copy the 4-bit condition register field
	     CR into the least significant end of register TARGET.  */
	  rtx scratch1 = gen_reg_rtx (SImode);
	  rtx scratch2 = gen_reg_rtx (SImode);
	  rtx subreg = simplify_gen_subreg (CCmode, scratch1, SImode, 0);
	  emit_insn (gen_movcc (subreg, cr));
	  emit_insn (gen_lshrsi3 (scratch2, scratch1, GEN_INT (28)));
	  emit_insn (gen_andsi3 (target, scratch2, GEN_INT (0xf)));
	}
    }

  if (nonvoid)
    return target;
  return const0_rtx;
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.
   Use the new builtin infrastructure.  */
rtx
rs6000_expand_builtin (tree exp, rtx target, rtx /* subtarget */,
		       machine_mode /* mode */, int ignore)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  enum rs6000_gen_builtins fcode
    = (enum rs6000_gen_builtins) DECL_MD_FUNCTION_CODE (fndecl);

  /* Emit error message if it's an unresolved overloaded builtin.  */
  if (fcode > RS6000_OVLD_NONE)
    {
      error ("unresolved overload for builtin %qF", fndecl);
      return const0_rtx;
    }

  size_t uns_fcode = (size_t)fcode;
  enum insn_code icode = rs6000_builtin_info[uns_fcode].icode;

  /* TODO: The following commentary and code is inherited from the original
     builtin processing code.  The commentary is a bit confusing, with the
     intent being that KFmode is always IEEE-128, IFmode is always IBM
     double-double, and TFmode is the current long double.  The code is
     confusing in that it converts from KFmode to TFmode pattern names,
     when the other direction is more intuitive.  Try to address this.  */

  /* We have two different modes (KFmode, TFmode) that are the IEEE
     128-bit floating point type, depending on whether long double is the
     IBM extended double (KFmode) or long double is IEEE 128-bit (TFmode).
     It is simpler if we only define one variant of the built-in function,
     and switch the code when defining it, rather than defining two built-
     ins and using the overload table in rs6000-c.cc to switch between the
     two.  If we don't have the proper assembler, don't do this switch
     because CODE_FOR_*kf* and CODE_FOR_*tf* will be CODE_FOR_nothing.  */
  if (FLOAT128_IEEE_P (TFmode))
    switch (icode)
      {
      case CODE_FOR_sqrtkf2_odd:
	icode = CODE_FOR_sqrttf2_odd;
	break;
      case CODE_FOR_trunckfdf2_odd:
	icode = CODE_FOR_trunctfdf2_odd;
	break;
      case CODE_FOR_addkf3_odd:
	icode = CODE_FOR_addtf3_odd;
	break;
      case CODE_FOR_subkf3_odd:
	icode = CODE_FOR_subtf3_odd;
	break;
      case CODE_FOR_mulkf3_odd:
	icode = CODE_FOR_multf3_odd;
	break;
      case CODE_FOR_divkf3_odd:
	icode = CODE_FOR_divtf3_odd;
	break;
      case CODE_FOR_fmakf4_odd:
	icode = CODE_FOR_fmatf4_odd;
	break;
      case CODE_FOR_xsxexpqp_kf_di:
	icode = CODE_FOR_xsxexpqp_tf_di;
	break;
      case CODE_FOR_xsxexpqp_kf_v2di:
	icode = CODE_FOR_xsxexpqp_tf_v2di;
	break;
      case CODE_FOR_xsxsigqp_kf_ti:
	icode = CODE_FOR_xsxsigqp_tf_ti;
	break;
      case CODE_FOR_xsxsigqp_kf_v1ti:
	icode = CODE_FOR_xsxsigqp_tf_v1ti;
	break;
      case CODE_FOR_xststdcnegqp_kf:
	icode = CODE_FOR_xststdcnegqp_tf;
	break;
      case CODE_FOR_xsiexpqp_kf_di:
	icode = CODE_FOR_xsiexpqp_tf_di;
	break;
      case CODE_FOR_xsiexpqp_kf_v2di:
	icode = CODE_FOR_xsiexpqp_tf_v2di;
	break;
      case CODE_FOR_xsiexpqpf_kf:
	icode = CODE_FOR_xsiexpqpf_tf;
	break;
      case CODE_FOR_xststdc_kf:
	icode = CODE_FOR_xststdc_tf;
	break;
      case CODE_FOR_xscmpexpqp_eq_kf:
	icode = CODE_FOR_xscmpexpqp_eq_tf;
	break;
      case CODE_FOR_xscmpexpqp_lt_kf:
	icode = CODE_FOR_xscmpexpqp_lt_tf;
	break;
      case CODE_FOR_xscmpexpqp_gt_kf:
	icode = CODE_FOR_xscmpexpqp_gt_tf;
	break;
      case CODE_FOR_xscmpexpqp_unordered_kf:
	icode = CODE_FOR_xscmpexpqp_unordered_tf;
	break;
      default:
	break;
      }

  /* In case of "#pragma target" changes, we initialize all builtins
     but check for actual availability now, during expand time.  For
     invalid builtins, generate a normal call.  */
  bifdata *bifaddr = &rs6000_builtin_info[uns_fcode];

  if (!rs6000_builtin_is_supported (fcode))
    {
      rs6000_invalid_builtin (fcode);
      return expand_call (exp, target, ignore);
    }

  if (bif_is_nosoft (*bifaddr)
      && rs6000_isa_flags & OPTION_MASK_SOFT_FLOAT)
    {
      error ("%qs not supported with %<-msoft-float%>",
	     bifaddr->bifname);
      return const0_rtx;
    }

  if (bif_is_no32bit (*bifaddr) && TARGET_32BIT)
    {
      error ("%qs is not supported in 32-bit mode", bifaddr->bifname);
      return const0_rtx;
    }

  if (bif_is_ibmld (*bifaddr) && !FLOAT128_2REG_P (TFmode))
    {
      error ("%qs requires %<long double%> to be IBM 128-bit format",
	     bifaddr->bifname);
      return const0_rtx;
    }

  if (bif_is_ibm128 (*bifaddr) && !ibm128_float_type_node)
    {
      error ("%qs requires %<__ibm128%> type support",
	     bifaddr->bifname);
      return const0_rtx;
    }

  if (bif_is_cpu (*bifaddr))
    return cpu_expand_builtin (fcode, exp, target);

  if (bif_is_extract (*bifaddr))
    return altivec_expand_vec_ext_builtin (exp, target);

  if (bif_is_predicate (*bifaddr))
    return altivec_expand_predicate_builtin (icode, exp, target);

  if (bif_is_htm (*bifaddr))
    return htm_expand_builtin (bifaddr, fcode, exp, target);

  if (bif_is_32bit (*bifaddr) && TARGET_32BIT)
    {
      if (fcode == RS6000_BIF_MFTB)
	icode = CODE_FOR_rs6000_mftb_si;
      else if (fcode == RS6000_BIF_BPERMD)
	icode = CODE_FOR_bpermd_si;
      else if (fcode == RS6000_BIF_DARN)
	icode = CODE_FOR_darn_64_si;
      else if (fcode == RS6000_BIF_DARN_32)
	icode = CODE_FOR_darn_32_si;
      else if (fcode == RS6000_BIF_DARN_RAW)
	icode = CODE_FOR_darn_raw_si;
      else
	gcc_unreachable ();
    }

  if (bif_is_endian (*bifaddr) && BYTES_BIG_ENDIAN)
    {
      if (fcode == RS6000_BIF_LD_ELEMREV_V1TI)
	icode = CODE_FOR_vsx_load_v1ti;
      else if (fcode == RS6000_BIF_LD_ELEMREV_V2DF)
	icode = CODE_FOR_vsx_load_v2df;
      else if (fcode == RS6000_BIF_LD_ELEMREV_V2DI)
	icode = CODE_FOR_vsx_load_v2di;
      else if (fcode == RS6000_BIF_LD_ELEMREV_V4SF)
	icode = CODE_FOR_vsx_load_v4sf;
      else if (fcode == RS6000_BIF_LD_ELEMREV_V4SI)
	icode = CODE_FOR_vsx_load_v4si;
      else if (fcode == RS6000_BIF_LD_ELEMREV_V8HI)
	icode = CODE_FOR_vsx_load_v8hi;
      else if (fcode == RS6000_BIF_LD_ELEMREV_V16QI)
	icode = CODE_FOR_vsx_load_v16qi;
      else if (fcode == RS6000_BIF_ST_ELEMREV_V1TI)
	icode = CODE_FOR_vsx_store_v1ti;
      else if (fcode == RS6000_BIF_ST_ELEMREV_V2DF)
	icode = CODE_FOR_vsx_store_v2df;
      else if (fcode == RS6000_BIF_ST_ELEMREV_V2DI)
	icode = CODE_FOR_vsx_store_v2di;
      else if (fcode == RS6000_BIF_ST_ELEMREV_V4SF)
	icode = CODE_FOR_vsx_store_v4sf;
      else if (fcode == RS6000_BIF_ST_ELEMREV_V4SI)
	icode = CODE_FOR_vsx_store_v4si;
      else if (fcode == RS6000_BIF_ST_ELEMREV_V8HI)
	icode = CODE_FOR_vsx_store_v8hi;
      else if (fcode == RS6000_BIF_ST_ELEMREV_V16QI)
	icode = CODE_FOR_vsx_store_v16qi;
      else if (fcode == RS6000_BIF_VCLZLSBB_V16QI)
	icode = CODE_FOR_vclzlsbb_v16qi;
      else if (fcode == RS6000_BIF_VCLZLSBB_V4SI)
	icode = CODE_FOR_vclzlsbb_v4si;
      else if (fcode == RS6000_BIF_VCLZLSBB_V8HI)
	icode = CODE_FOR_vclzlsbb_v8hi;
      else if (fcode == RS6000_BIF_VCTZLSBB_V16QI)
	icode = CODE_FOR_vctzlsbb_v16qi;
      else if (fcode == RS6000_BIF_VCTZLSBB_V4SI)
	icode = CODE_FOR_vctzlsbb_v4si;
      else if (fcode == RS6000_BIF_VCTZLSBB_V8HI)
	icode = CODE_FOR_vctzlsbb_v8hi;
      else
	gcc_unreachable ();
    }

  if (bif_is_ibm128 (*bifaddr) && TARGET_LONG_DOUBLE_128 && !TARGET_IEEEQUAD)
    {
      if (fcode == RS6000_BIF_PACK_IF)
	{
	  icode = CODE_FOR_packtf;
	  fcode = RS6000_BIF_PACK_TF;
	  uns_fcode = (size_t) fcode;
	}
      else if (fcode == RS6000_BIF_UNPACK_IF)
	{
	  icode = CODE_FOR_unpacktf;
	  fcode = RS6000_BIF_UNPACK_TF;
	  uns_fcode = (size_t) fcode;
	}
    }

  /* TRUE iff the built-in function returns void.  */
  bool void_func = TREE_TYPE (TREE_TYPE (fndecl)) == void_type_node;
  /* Position of first argument (0 for void-returning functions, else 1).  */
  int k;
  /* Modes for the return value, if any, and arguments.  */
  const int MAX_BUILTIN_ARGS = 6;
  machine_mode mode[MAX_BUILTIN_ARGS + 1];

  if (void_func)
    k = 0;
  else
    {
      k = 1;
      mode[0] = insn_data[icode].operand[0].mode;
    }

  /* Tree expressions for each argument.  */
  tree arg[MAX_BUILTIN_ARGS];
  /* RTL expressions for each argument.  */
  rtx op[MAX_BUILTIN_ARGS];

  int nargs = bifaddr->nargs;
  gcc_assert (nargs <= MAX_BUILTIN_ARGS);


  for (int i = 0; i < nargs; i++)
    {
      arg[i] = CALL_EXPR_ARG (exp, i);
      if (arg[i] == error_mark_node)
	return const0_rtx;
      STRIP_NOPS (arg[i]);
      op[i] = expand_normal (arg[i]);
      /* We have a couple of pesky patterns that don't specify the mode...  */
      mode[i+k] = insn_data[icode].operand[i+k].mode;
      if (!mode[i+k])
	mode[i+k] = Pmode;
    }

  /* Check for restricted constant arguments.  */
  for (int i = 0; i < 2; i++)
    {
      switch (bifaddr->restr[i])
	{
	case RES_BITS:
	  {
	    size_t mask = 1;
	    mask <<= bifaddr->restr_val1[i];
	    mask--;
	    tree restr_arg = arg[bifaddr->restr_opnd[i] - 1];
	    STRIP_NOPS (restr_arg);
	    if (!(TREE_CODE (restr_arg) == INTEGER_CST
		  && (TREE_INT_CST_LOW (restr_arg) & ~mask) == 0))
	      {
		unsigned p = (1U << bifaddr->restr_val1[i]) - 1;
		error ("argument %d must be a literal between 0 and %d,"
		       " inclusive",
		       bifaddr->restr_opnd[i], p);
		return CONST0_RTX (mode[0]);
	      }
	    break;
	  }
	case RES_RANGE:
	  {
	    tree restr_arg = arg[bifaddr->restr_opnd[i] - 1];
	    STRIP_NOPS (restr_arg);
	    if (!(TREE_CODE (restr_arg) == INTEGER_CST
		  && IN_RANGE (tree_to_shwi (restr_arg),
			       bifaddr->restr_val1[i],
			       bifaddr->restr_val2[i])))
	      {
		error ("argument %d must be a literal between %d and %d,"
		       " inclusive",
		       bifaddr->restr_opnd[i], bifaddr->restr_val1[i],
		       bifaddr->restr_val2[i]);
		return CONST0_RTX (mode[0]);
	      }
	    break;
	  }
	case RES_VAR_RANGE:
	  {
	    tree restr_arg = arg[bifaddr->restr_opnd[i] - 1];
	    STRIP_NOPS (restr_arg);
	    if (TREE_CODE (restr_arg) == INTEGER_CST
		&& !IN_RANGE (tree_to_shwi (restr_arg),
			      bifaddr->restr_val1[i],
			      bifaddr->restr_val2[i]))
	      {
		error ("argument %d must be a variable or a literal "
		       "between %d and %d, inclusive",
		       bifaddr->restr_opnd[i], bifaddr->restr_val1[i],
		       bifaddr->restr_val2[i]);
		return CONST0_RTX (mode[0]);
	      }
	    break;
	  }
	case RES_VALUES:
	  {
	    tree restr_arg = arg[bifaddr->restr_opnd[i] - 1];
	    STRIP_NOPS (restr_arg);
	    if (!(TREE_CODE (restr_arg) == INTEGER_CST
		  && (tree_to_shwi (restr_arg) == bifaddr->restr_val1[i]
		      || tree_to_shwi (restr_arg) == bifaddr->restr_val2[i])))
	      {
		error ("argument %d must be either a literal %d or a "
		       "literal %d",
		       bifaddr->restr_opnd[i], bifaddr->restr_val1[i],
		       bifaddr->restr_val2[i]);
		return CONST0_RTX (mode[0]);
	      }
	    break;
	  }
	default:
	case RES_NONE:
	  break;
	}
    }

  if (bif_is_ldstmask (*bifaddr))
    return rs6000_expand_ldst_mask (target, arg[0]);

  if (bif_is_stvec (*bifaddr))
    {
      if (bif_is_reve (*bifaddr))
	icode = elemrev_icode (fcode);
      return stv_expand_builtin (icode, op, mode[0], mode[1]);
    }

  if (bif_is_ldvec (*bifaddr))
    {
      if (bif_is_reve (*bifaddr))
	icode = elemrev_icode (fcode);
      return ldv_expand_builtin (target, icode, op, mode[0]);
    }

  if (bif_is_lxvrse (*bifaddr))
    return lxvrse_expand_builtin (target, icode, op, mode[0], mode[1]);

  if (bif_is_lxvrze (*bifaddr))
    return lxvrze_expand_builtin (target, icode, op, mode[0], mode[1]);

  if (bif_is_mma (*bifaddr))
    return mma_expand_builtin (exp, target, icode, fcode);

  if (TREE_TYPE (TREE_TYPE (fndecl)) == void_type_node)
    target = NULL_RTX;
  else if (target == 0
	   || GET_MODE (target) != mode[0]
	   || !insn_data[icode].operand[0].predicate (target, mode[0]))
    target = gen_reg_rtx (mode[0]);

  for (int i = 0; i < nargs; i++)
    if (!insn_data[icode].operand[i+k].predicate (op[i], mode[i+k]))
      op[i] = copy_to_mode_reg (mode[i+k], op[i]);

  rtx pat;

  switch (nargs)
    {
    case 0:
      pat = (void_func
	     ? GEN_FCN (icode) ()
	     : GEN_FCN (icode) (target));
      break;
    case 1:
      pat = (void_func
	     ? GEN_FCN (icode) (op[0])
	     : GEN_FCN (icode) (target, op[0]));
      break;
    case 2:
      pat = (void_func
	     ? GEN_FCN (icode) (op[0], op[1])
	     : GEN_FCN (icode) (target, op[0], op[1]));
      break;
    case 3:
      pat = (void_func
	     ? GEN_FCN (icode) (op[0], op[1], op[2])
	     : GEN_FCN (icode) (target, op[0], op[1], op[2]));
      break;
    case 4:
      pat = (void_func
	     ? GEN_FCN (icode) (op[0], op[1], op[2], op[3])
	     : GEN_FCN (icode) (target, op[0], op[1], op[2], op[3]));
      break;
    case 5:
      pat = (void_func
	     ? GEN_FCN (icode) (op[0], op[1], op[2], op[3], op[4])
	     : GEN_FCN (icode) (target, op[0], op[1], op[2], op[3], op[4]));
      break;
    case 6:
      pat = (void_func
	     ? GEN_FCN (icode) (op[0], op[1], op[2], op[3], op[4], op[5])
	     : GEN_FCN (icode) (target, op[0], op[1],
				op[2], op[3], op[4], op[5]));
      break;
    default:
      gcc_assert (MAX_BUILTIN_ARGS == 6);
      gcc_unreachable ();
    }

  if (!pat)
    return 0;

  emit_insn (pat);
  return target;
}
