/* Target-specific code for C family languages.
   Copyright (C) 2015-2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "input.h"
#include "memmodel.h"
#include "tm_p.h"
#include "flags.h"
#include "c-family/c-common.h"
#include "cpplib.h"
#include "c-family/c-pragma.h"
#include "langhooks.h"
#include "target.h"


#define builtin_define(TXT) cpp_define (pfile, TXT)
#define builtin_assert(TXT) cpp_assert (pfile, TXT)


static void
aarch64_def_or_undef (bool def_p, const char *macro, cpp_reader *pfile)
{
  if (def_p)
    cpp_define (pfile, macro);
  else
    cpp_undef (pfile, macro);
}

/* Define the macros that we always expect to have on AArch64.  */

static void
aarch64_define_unconditional_macros (cpp_reader *pfile)
{
  builtin_define ("__aarch64__");
  builtin_define ("__ARM_64BIT_STATE");

  builtin_define ("__ARM_ARCH_ISA_A64");
  builtin_define_with_int_value ("__ARM_ALIGN_MAX_PWR", 28);
  builtin_define_with_int_value ("__ARM_ALIGN_MAX_STACK_PWR", 16);

  /* __ARM_ARCH_8A is not mandated by ACLE but we define it unconditionally
     as interoperability with the same arm macro.  */
  builtin_define ("__ARM_ARCH_8A");

  builtin_define_with_int_value ("__ARM_ARCH_PROFILE", 'A');
  builtin_define ("__ARM_FEATURE_CLZ");
  builtin_define ("__ARM_FEATURE_IDIV");
  builtin_define ("__ARM_FEATURE_UNALIGNED");
  builtin_define ("__ARM_PCS_AAPCS64");
  builtin_define_with_int_value ("__ARM_SIZEOF_WCHAR_T", WCHAR_TYPE_SIZE / 8);

  builtin_define ("__GCC_ASM_FLAG_OUTPUTS__");
}

/* Undefine/redefine macros that depend on the current backend state and may
   need to change when a target pragma modifies the backend state.  */

static void
aarch64_update_cpp_builtins (cpp_reader *pfile)
{
  aarch64_def_or_undef (flag_unsafe_math_optimizations, "__ARM_FP_FAST", pfile);

  builtin_define_with_int_value ("__ARM_ARCH", aarch64_architecture_version);

  builtin_define_with_int_value ("__ARM_SIZEOF_MINIMAL_ENUM",
				 flag_short_enums ? 1 : 4);
  aarch64_def_or_undef (TARGET_BIG_END, "__AARCH64EB__", pfile);
  aarch64_def_or_undef (TARGET_BIG_END, "__ARM_BIG_ENDIAN", pfile);
  aarch64_def_or_undef (!TARGET_BIG_END, "__AARCH64EL__", pfile);

  aarch64_def_or_undef (TARGET_FLOAT, "__ARM_FEATURE_FMA", pfile);

  if (TARGET_FLOAT || TARGET_SIMD)
    {
      builtin_define_with_int_value ("__ARM_FP", 0x0E);
      builtin_define ("__ARM_FP16_FORMAT_IEEE");
      builtin_define ("__ARM_FP16_ARGS");
    }
  else
    cpp_undef (pfile, "__ARM_FP");

  aarch64_def_or_undef (TARGET_FP_F16INST,
			"__ARM_FEATURE_FP16_SCALAR_ARITHMETIC", pfile);
  aarch64_def_or_undef (TARGET_SIMD_F16INST,
			"__ARM_FEATURE_FP16_VECTOR_ARITHMETIC", pfile);

  aarch64_def_or_undef (TARGET_SIMD, "__ARM_FEATURE_NUMERIC_MAXMIN", pfile);
  aarch64_def_or_undef (TARGET_SIMD, "__ARM_NEON", pfile);


  aarch64_def_or_undef (TARGET_CRC32, "__ARM_FEATURE_CRC32", pfile);
  aarch64_def_or_undef (TARGET_DOTPROD, "__ARM_FEATURE_DOTPROD", pfile);
  aarch64_def_or_undef (TARGET_COMPLEX, "__ARM_FEATURE_COMPLEX", pfile);
  aarch64_def_or_undef (TARGET_JSCVT, "__ARM_FEATURE_JCVT", pfile);

  cpp_undef (pfile, "__AARCH64_CMODEL_TINY__");
  cpp_undef (pfile, "__AARCH64_CMODEL_SMALL__");
  cpp_undef (pfile, "__AARCH64_CMODEL_LARGE__");

  switch (aarch64_cmodel)
    {
      case AARCH64_CMODEL_TINY:
      case AARCH64_CMODEL_TINY_PIC:
	builtin_define ("__AARCH64_CMODEL_TINY__");
	break;
      case AARCH64_CMODEL_SMALL:
      case AARCH64_CMODEL_SMALL_PIC:
	builtin_define ("__AARCH64_CMODEL_SMALL__");
	break;
      case AARCH64_CMODEL_LARGE:
	builtin_define ("__AARCH64_CMODEL_LARGE__");
	break;
      default:
	break;
    }

  aarch64_def_or_undef (TARGET_ILP32, "_ILP32", pfile);
  aarch64_def_or_undef (TARGET_ILP32, "__ILP32__", pfile);

  aarch64_def_or_undef (TARGET_CRYPTO, "__ARM_FEATURE_CRYPTO", pfile);
  aarch64_def_or_undef (TARGET_SIMD_RDMA, "__ARM_FEATURE_QRDMX", pfile);
  aarch64_def_or_undef (TARGET_SVE, "__ARM_FEATURE_SVE", pfile);
  cpp_undef (pfile, "__ARM_FEATURE_SVE_BITS");
  if (TARGET_SVE)
    {
      int bits;
      if (!BITS_PER_SVE_VECTOR.is_constant (&bits))
	bits = 0;
      builtin_define_with_int_value ("__ARM_FEATURE_SVE_BITS", bits);
    }
  aarch64_def_or_undef (TARGET_SVE2, "__ARM_FEATURE_SVE2", pfile);

  aarch64_def_or_undef (TARGET_LSE, "__ARM_FEATURE_ATOMICS", pfile);
  aarch64_def_or_undef (TARGET_AES, "__ARM_FEATURE_AES", pfile);
  aarch64_def_or_undef (TARGET_SHA2, "__ARM_FEATURE_SHA2", pfile);
  aarch64_def_or_undef (TARGET_SHA3, "__ARM_FEATURE_SHA3", pfile);
  aarch64_def_or_undef (TARGET_SHA3, "__ARM_FEATURE_SHA512", pfile);
  aarch64_def_or_undef (TARGET_SM4, "__ARM_FEATURE_SM3", pfile);
  aarch64_def_or_undef (TARGET_SM4, "__ARM_FEATURE_SM4", pfile);
  aarch64_def_or_undef (TARGET_F16FML, "__ARM_FEATURE_FP16_FML", pfile);

  aarch64_def_or_undef (TARGET_FRINT, "__ARM_FEATURE_FRINT", pfile);
  aarch64_def_or_undef (TARGET_TME, "__ARM_FEATURE_TME", pfile);
  aarch64_def_or_undef (TARGET_RNG, "__ARM_FEATURE_RNG", pfile);
  aarch64_def_or_undef (TARGET_MEMTAG, "__ARM_FEATURE_MEMORY_TAGGING", pfile);

  /* Not for ACLE, but required to keep "float.h" correct if we switch
     target between implementations that do or do not support ARMv8.2-A
     16-bit floating-point extensions.  */
  cpp_undef (pfile, "__FLT_EVAL_METHOD__");
  builtin_define_with_int_value ("__FLT_EVAL_METHOD__",
				 c_flt_eval_method (true));
  cpp_undef (pfile, "__FLT_EVAL_METHOD_C99__");
  builtin_define_with_int_value ("__FLT_EVAL_METHOD_C99__",
				 c_flt_eval_method (false));
}

/* Implement TARGET_CPU_CPP_BUILTINS.  */

void
aarch64_cpu_cpp_builtins (cpp_reader *pfile)
{
  aarch64_define_unconditional_macros (pfile);
  aarch64_update_cpp_builtins (pfile);
}

/* Hook to validate the current #pragma GCC target and set the state, and
   update the macros based on what was changed.  If ARGS is NULL, then
   POP_TARGET is used to reset the options.  */

static bool
aarch64_pragma_target_parse (tree args, tree pop_target)
{
  /* If args is not NULL then process it and setup the target-specific
     information that it specifies.  */
  if (args)
    {
      if (!aarch64_process_target_attr (args))
	return false;

      aarch64_override_options_internal (&global_options);
    }

  /* args is NULL, restore to the state described in pop_target.  */
  else
    {
      pop_target = pop_target ? pop_target : target_option_default_node;
      cl_target_option_restore (&global_options,
				TREE_TARGET_OPTION (pop_target));
    }

  target_option_current_node
    = build_target_option_node (&global_options);

  aarch64_reset_previous_fndecl ();
  /* For the definitions, ensure all newly defined macros are considered
     as used for -Wunused-macros.  There is no point warning about the
     compiler predefined macros.  */
  cpp_options *cpp_opts = cpp_get_options (parse_in);
  unsigned char saved_warn_unused_macros = cpp_opts->warn_unused_macros;
  cpp_opts->warn_unused_macros = 0;

  aarch64_update_cpp_builtins (parse_in);

  cpp_opts->warn_unused_macros = saved_warn_unused_macros;

  /* If we're popping or reseting make sure to update the globals so that
     the optab availability predicates get recomputed.  */
  if (pop_target)
    aarch64_save_restore_target_globals (pop_target);

  /* Initialize SIMD builtins if we haven't already.
     Set current_target_pragma to NULL for the duration so that
     the builtin initialization code doesn't try to tag the functions
     being built with the attributes specified by any current pragma, thus
     going into an infinite recursion.  */
  if (TARGET_SIMD)
    {
      tree saved_current_target_pragma = current_target_pragma;
      current_target_pragma = NULL;
      aarch64_init_simd_builtins ();
      current_target_pragma = saved_current_target_pragma;
    }

  return true;
}

/* Implement "#pragma GCC aarch64".  */
static void
aarch64_pragma_aarch64 (cpp_reader *)
{
  tree x;
  if (pragma_lex (&x) != CPP_STRING)
    {
      error ("%<#pragma GCC aarch64%> requires a string parameter");
      return;
    }

  const char *name = TREE_STRING_POINTER (x);
  if (strcmp (name, "arm_sve.h") == 0)
    aarch64_sve::handle_arm_sve_h ();
  else
    error ("unknown %<#pragma GCC aarch64%> option %qs", name);
}

/* Implement TARGET_RESOLVE_OVERLOADED_BUILTIN.  */
static tree
aarch64_resolve_overloaded_builtin (unsigned int uncast_location,
				    tree fndecl, void *uncast_arglist)
{
  vec<tree, va_gc> empty = {};
  location_t location = (location_t) uncast_location;
  vec<tree, va_gc> *arglist = (uncast_arglist
			       ? (vec<tree, va_gc> *) uncast_arglist
			       : &empty);
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  tree new_fndecl;
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      return aarch64_resolve_overloaded_builtin_general (location, fndecl,
							 uncast_arglist);
    case AARCH64_BUILTIN_SVE:
      new_fndecl = aarch64_sve::resolve_overloaded_builtin (location, subcode,
							    arglist);
      break;
    }
  if (new_fndecl == NULL_TREE || new_fndecl == error_mark_node)
    return new_fndecl;
  return build_function_call_vec (location, vNULL, new_fndecl, arglist,
				  NULL, fndecl);
}

/* Implement TARGET_CHECK_BUILTIN_CALL.  */
static bool
aarch64_check_builtin_call (location_t loc, vec<location_t> arg_loc,
			    tree fndecl, tree orig_fndecl,
			    unsigned int nargs, tree *args)
{
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      return true;

    case AARCH64_BUILTIN_SVE:
      return aarch64_sve::check_builtin_call (loc, arg_loc, subcode,
					      orig_fndecl, nargs, args);
    }
  gcc_unreachable ();
}

/* Implement REGISTER_TARGET_PRAGMAS.  */

void
aarch64_register_pragmas (void)
{
  /* Update pragma hook to allow parsing #pragma GCC target.  */
  targetm.target_option.pragma_parse = aarch64_pragma_target_parse;

  targetm.resolve_overloaded_builtin = aarch64_resolve_overloaded_builtin;
  targetm.check_builtin_call = aarch64_check_builtin_call;

  c_register_pragma ("GCC", "aarch64", aarch64_pragma_aarch64);
}
