/* Copyright (C) 2007-2025 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "c-family/c-common.h"
#include "memmodel.h"
#include "tm_p.h"
#include "c-family/c-pragma.h"
#include "stringpool.h"
#include "arm-builtins.h"
#include "arm-protos.h"

tree
arm_resolve_cde_builtin (location_t loc, tree fndecl, void *arglist)
{
  vec<tree, va_gc> *params = static_cast<vec<tree, va_gc> *> (arglist);
  unsigned param_num = params ? params->length() : 0;
  unsigned num_args = list_length (TYPE_ARG_TYPES (TREE_TYPE (fndecl))) - 1;
  /* Ensure this function has the correct number of arguments.
     This won't happen when using the intrinsics defined by the ACLE, since
     they're exposed to the user via a wrapper in the arm_cde.h header that has
     the correct number of arguments ... hence the compiler would already catch
     an incorrect number of arguments there.

     It is still possible to get here if the user tries to call the __bulitin_*
     functions directly.  We could print some error message in this function,
     but instead we leave it to the rest of the code to catch this problem in
     the same way that other __builtin_* functions catch it.

     This does mean an odd error message, but it's consistent with the rest of
     the builtins.  */
  if (param_num != num_args)
    return NULL_TREE;

  tree to_return = NULL_TREE;
  /* Take the functions return type since that's the same type as the arguments
     this function needs (the types of the builtin function all come from the
     machine mode of the RTL pattern, and they're all the same and calculated
     in the same way).  */
  tree pattern_type = TREE_TYPE (TREE_TYPE (fndecl));

  unsigned i;
  /* Hard coding the number of parameters we don't want to cast at the end of
     the builtin.  This is the  easiest approach for the CDE intrinsics, and
     introducing a parameter to store in the builtins.def macros seems overkill
     when they're only relevant here.  */
  unsigned end_args = arm_cde_end_args (fndecl);
  unsigned cast_param_end = param_num - end_args;
  /* For the vcx1q patterns that don't need any casts.  */
  if (cast_param_end == 1)
    return NULL_TREE;

  /* In order to check all arguments rather than complaining on the first
     invalid one we record whether *any* arguments are invalid using this
     boolean variable.  */
  bool invalid = false;
  for (i = 1; i < cast_param_end; i++)
    {
      tree this_param = (*params)[i];
      if (TREE_CODE (this_param) == ERROR_MARK)
	{
	  invalid = true;
	  continue;
	}
      tree param_type = TREE_TYPE (this_param);

      /* Return value is cast to type that second argument originally was.
	 All non-constant arguments are cast to the return type calculated from
	 the RTL pattern.

	 Set the return type to an unqualified version of the type of the first
	 parameter.  The first parameter since that is how the intrinsics are
	 defined -- to always return the same type as the first polymorphic
	 argument.  Unqualified version of the type since we don't want passing
	 a constant parameter to mean that the return value of the builtin is
	 also constant.  */
      if (i == 1)
	to_return = build_qualified_type (param_type, 0 MEM_STAT_INFO);

      /* The only requirement of these intrinsics on the type of the variable
	 is that it's 128 bits wide.  All other types are valid and we simply
	 VIEW_CONVERT_EXPR them to the type of the underlying builtin.  */
      tree type_size = TYPE_SIZE (param_type);
      if (! tree_fits_shwi_p (type_size)
	  || tree_to_shwi (type_size) != 128)
	{
	  error_at (loc,
		    "argument %u to function %qE is of type %qT which is not "
		    "known to be 128 bits wide",
		    i + 1, fndecl, param_type);
	  invalid = true;
	  continue;
	}

      /* Only convert the argument if we actually need to.  */
      if (! check_base_type (pattern_type, param_type))
	(*params)[i] = build1 (VIEW_CONVERT_EXPR, pattern_type, this_param);
    }
  if (invalid)
    return NULL_TREE;

  /* We know it's safe to call this since this builtin is here to implement an
     ACLE function, and those functions are only for C/C++.  */
  tree call_expr = build_function_call_vec (loc, vNULL, fndecl, params,
					    NULL, fndecl);

  gcc_assert (to_return != NULL_TREE);
  if (! check_base_type (to_return, pattern_type))
    return build1 (VIEW_CONVERT_EXPR, to_return, call_expr);
  return call_expr;
}

/* Implement "#pragma GCC arm".  */
static void
arm_pragma_arm (cpp_reader *)
{
  tree x;
  if (pragma_lex (&x) != CPP_STRING)
    {
      error ("%<#pragma GCC arm%> requires a string parameter");
      return;
    }

  const char *name = TREE_STRING_POINTER (x);
  if (strcmp (name, "arm_mve_types.h") == 0)
    arm_mve::handle_arm_mve_types_h ();
  else if (strcmp (name, "arm_mve.h") == 0)
    {
      if (pragma_lex (&x) == CPP_NAME)
	{
	  if (strcmp (IDENTIFIER_POINTER (x), "true") == 0)
	    arm_mve::handle_arm_mve_h (true);
	  else if (strcmp (IDENTIFIER_POINTER (x), "false") == 0)
	    arm_mve::handle_arm_mve_h (false);
	  else
	    error ("%<#pragma GCC arm \"arm_mve.h\"%> requires a boolean parameter");
	}
    }
  else
    error ("unknown %<#pragma GCC arm%> option %qs", name);
}

/* Implement TARGET_RESOLVE_OVERLOADED_BUILTIN.  */
tree
arm_resolve_overloaded_builtin (location_t loc, tree fndecl,
				void *uncast_arglist, bool)
{
  enum resolver_ident resolver = arm_describe_resolver (fndecl);
  if (resolver == arm_cde_resolver)
    return arm_resolve_cde_builtin (loc, fndecl, uncast_arglist);
  if (resolver == arm_mve_resolver)
    {
      vec<tree, va_gc> empty = {};
      vec<tree, va_gc> *arglist = (uncast_arglist
				   ? (vec<tree, va_gc> *) uncast_arglist
				   : &empty);
      unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
      unsigned int subcode = code >> ARM_BUILTIN_SHIFT;
      tree new_fndecl = arm_mve::resolve_overloaded_builtin (loc, subcode, arglist);
      if (new_fndecl == NULL_TREE || new_fndecl == error_mark_node)
	return new_fndecl;
      return build_function_call_vec (loc, vNULL, new_fndecl, arglist,
				      NULL, fndecl);
    }
  return NULL_TREE;
}

/* Output C specific EABI object attributes.  These cannot be done in
   arm.cc because they require information from the C frontend.  */

static void
arm_output_c_attributes (void)
{
  int wchar_size = (int)(TYPE_PRECISION (wchar_type_node) / BITS_PER_UNIT);
  arm_emit_eabi_attribute ("Tag_ABI_PCS_wchar_t", 18, wchar_size);
}


/* Setup so that common code calls arm_output_c_attributes.  */

void
arm_lang_object_attributes_init (void)
{
  arm_lang_output_object_attributes_hook = arm_output_c_attributes;
}

#define builtin_define(TXT) cpp_define (pfile, TXT)
#define builtin_assert(TXT) cpp_assert (pfile, TXT)

/* Define or undefine macros based on the current target.  If the user does
   #pragma GCC target, we need to adjust the macros dynamically.  */

static void
def_or_undef_macro(struct cpp_reader* pfile, const char *name, bool def_p)
{
  if (def_p)
    cpp_define (pfile, name);
  else
    cpp_undef (pfile, name);
}

static void
arm_cpu_builtins (struct cpp_reader* pfile)
{
  def_or_undef_macro (pfile, "__ARM_FEATURE_DSP", TARGET_DSP_MULTIPLY);
  def_or_undef_macro (pfile, "__ARM_FEATURE_QBIT", TARGET_ARM_QBIT);
  def_or_undef_macro (pfile, "__ARM_FEATURE_SAT", TARGET_ARM_SAT);
  def_or_undef_macro (pfile, "__ARM_FEATURE_CRYPTO", TARGET_CRYPTO);
  def_or_undef_macro (pfile, "__ARM_FEATURE_AES", TARGET_CRYPTO);
  def_or_undef_macro (pfile, "__ARM_FEATURE_SHA2", TARGET_CRYPTO);

  def_or_undef_macro (pfile, "__ARM_FEATURE_UNALIGNED", unaligned_access);

  def_or_undef_macro (pfile, "__ARM_FEATURE_QRDMX", TARGET_NEON_RDMA);

  def_or_undef_macro (pfile, "__ARM_FEATURE_CRC32", TARGET_CRC32);
  def_or_undef_macro (pfile, "__ARM_FEATURE_DOTPROD", TARGET_DOTPROD);
  def_or_undef_macro (pfile, "__ARM_FEATURE_COMPLEX", TARGET_COMPLEX);
  def_or_undef_macro (pfile, "__ARM_32BIT_STATE", TARGET_32BIT);

  def_or_undef_macro (pfile, "__ARM_FEATURE_PAUTH", TARGET_HAVE_PACBTI);
  def_or_undef_macro (pfile, "__ARM_FEATURE_BTI", TARGET_HAVE_PACBTI);
  def_or_undef_macro (pfile, "__ARM_FEATURE_BTI_DEFAULT",
		      aarch_enable_bti == 1);

  cpp_undef (pfile, "__ARM_FEATURE_PAC_DEFAULT");
  if (aarch_ra_sign_scope != AARCH_FUNCTION_NONE)
  {
    unsigned int pac = 1;

    if (aarch_ra_sign_scope == AARCH_FUNCTION_ALL)
      pac |= 0x4;

    builtin_define_with_int_value ("__ARM_FEATURE_PAC_DEFAULT", pac);
  }

  cpp_undef (pfile, "__ARM_FEATURE_MVE");
  if (TARGET_HAVE_MVE && TARGET_HAVE_MVE_FLOAT)
    {
      builtin_define_with_int_value ("__ARM_FEATURE_MVE", 3);
    }
  else if (TARGET_HAVE_MVE)
    {
      builtin_define_with_int_value ("__ARM_FEATURE_MVE", 1);
    }

  cpp_undef (pfile, "__ARM_FEATURE_CMSE");
  if (arm_arch8 && !arm_arch_notm)
    {
      if (arm_arch_cmse && use_cmse)
	builtin_define_with_int_value ("__ARM_FEATURE_CMSE", 3);
      else
	builtin_define ("__ARM_FEATURE_CMSE");
    }

  cpp_undef (pfile, "__ARM_FEATURE_LDREX");
  if (TARGET_ARM_FEATURE_LDREX)
    builtin_define_with_int_value ("__ARM_FEATURE_LDREX",
				   TARGET_ARM_FEATURE_LDREX);

  /* ACLE says that __ARM_FEATURE_CLZ is defined if the hardware
     supports it; it's also clear that this doesn't mean the current
     ISA, so we define this even when compiling for Thumb1 if the
     target supports CLZ in A32.  */
  def_or_undef_macro (pfile, "__ARM_FEATURE_CLZ",
		      ((TARGET_ARM_ARCH >= 5 && arm_arch_notm)
		       || TARGET_ARM_ARCH_ISA_THUMB >=2));

  def_or_undef_macro (pfile, "__ARM_FEATURE_NUMERIC_MAXMIN",
		      TARGET_ARM_ARCH >= 8 && TARGET_NEON && TARGET_VFP5);

  def_or_undef_macro (pfile, "__ARM_FEATURE_SIMD32", TARGET_INT_SIMD);

  builtin_define_with_int_value ("__ARM_SIZEOF_MINIMAL_ENUM",
				 flag_short_enums ? 1 : 4);
  builtin_define_type_sizeof ("__ARM_SIZEOF_WCHAR_T", wchar_type_node);

  cpp_undef (pfile, "__ARM_ARCH_PROFILE");
  if (TARGET_ARM_ARCH_PROFILE)
    builtin_define_with_int_value ("__ARM_ARCH_PROFILE",
				   TARGET_ARM_ARCH_PROFILE);

  /* Define __arm__ even when in thumb mode, for
     consistency with armcc.  */
  builtin_define ("__arm__");
  if (TARGET_ARM_ARCH)
    {
      cpp_undef (pfile, "__ARM_ARCH");
      builtin_define_with_int_value ("__ARM_ARCH", TARGET_ARM_ARCH);
    }
  if (arm_arch_notm)
    builtin_define ("__ARM_ARCH_ISA_ARM");
  builtin_define ("__APCS_32__");

  def_or_undef_macro (pfile, "__GCC_ASM_FLAG_OUTPUTS__", !TARGET_THUMB1);

  def_or_undef_macro (pfile, "__thumb__", TARGET_THUMB);
  def_or_undef_macro (pfile, "__thumb2__", TARGET_THUMB2);
  if (TARGET_BIG_END)
    def_or_undef_macro (pfile, "__THUMBEB__", TARGET_THUMB);
  else
    def_or_undef_macro (pfile, "__THUMBEL__", TARGET_THUMB);

  cpp_undef (pfile, "__ARM_ARCH_ISA_THUMB");
  if (TARGET_ARM_ARCH_ISA_THUMB)
    builtin_define_with_int_value ("__ARM_ARCH_ISA_THUMB",
				   TARGET_ARM_ARCH_ISA_THUMB);

  if (TARGET_BIG_END)
    {
      builtin_define ("__ARMEB__");
      builtin_define ("__ARM_BIG_ENDIAN");
    }
  else
    {
      builtin_define ("__ARMEL__");
    }

  if (TARGET_SOFT_FLOAT)
    builtin_define ("__SOFTFP__");

  builtin_define ("__VFP_FP__");

  cpp_undef (pfile, "__ARM_FP");
  if (TARGET_ARM_FP)
    builtin_define_with_int_value ("__ARM_FP", TARGET_ARM_FP);

  def_or_undef_macro (pfile, "__ARM_FP16_FORMAT_IEEE",
		      arm_fp16_format == ARM_FP16_FORMAT_IEEE);
  def_or_undef_macro (pfile, "__ARM_FP16_FORMAT_ALTERNATIVE",
		      arm_fp16_format == ARM_FP16_FORMAT_ALTERNATIVE);
  def_or_undef_macro (pfile, "__ARM_FP16_ARGS",
		      arm_fp16_format != ARM_FP16_FORMAT_NONE);

  def_or_undef_macro (pfile, "__ARM_FEATURE_FP16_SCALAR_ARITHMETIC",
		      TARGET_VFP_FP16INST);
  def_or_undef_macro (pfile, "__ARM_FEATURE_FP16_VECTOR_ARITHMETIC",
		      TARGET_NEON_FP16INST);
  def_or_undef_macro (pfile, "__ARM_FEATURE_FP16_FML", TARGET_FP16FML);

  def_or_undef_macro (pfile, "__ARM_FEATURE_FMA", TARGET_FMA);
  def_or_undef_macro (pfile, "__ARM_NEON__", TARGET_NEON);
  def_or_undef_macro (pfile, "__ARM_NEON", TARGET_NEON);

  cpp_undef (pfile, "__ARM_NEON_FP");
  if (TARGET_NEON_FP)
    builtin_define_with_int_value ("__ARM_NEON_FP", TARGET_NEON_FP);

  /* Add a define for interworking. Needed when building libgcc.a.  */
  if (arm_cpp_interwork)
    builtin_define ("__THUMB_INTERWORK__");

  builtin_define (arm_arch_name);
  if (arm_arch_xscale)
    builtin_define ("__XSCALE__");
  if (arm_arch_iwmmxt)
    {
      builtin_define ("__IWMMXT__");
      builtin_define ("__ARM_WMMX");
    }
  if (arm_arch_iwmmxt2)
    builtin_define ("__IWMMXT2__");
  /* ARMv6KZ was originally identified as the misspelled __ARM_ARCH_6ZK__.  To
     preserve the existing behavior, the misspelled feature macro must still be
     defined.  */
  if (arm_arch6kz)
    builtin_define ("__ARM_ARCH_6ZK__");
  if (TARGET_AAPCS_BASED)
    {
      if (arm_pcs_default == ARM_PCS_AAPCS_VFP)
	builtin_define ("__ARM_PCS_VFP");
      else if (arm_pcs_default == ARM_PCS_AAPCS)
	builtin_define ("__ARM_PCS");
      builtin_define ("__ARM_EABI__");
    }

  def_or_undef_macro (pfile, "__FDPIC__", TARGET_FDPIC);

  def_or_undef_macro (pfile, "__ARM_ARCH_EXT_IDIV__", TARGET_IDIV);
  def_or_undef_macro (pfile, "__ARM_FEATURE_IDIV", TARGET_IDIV);

  def_or_undef_macro (pfile, "__ARM_ASM_SYNTAX_UNIFIED__", inline_asm_unified);

  cpp_undef (pfile, "__ARM_FEATURE_COPROC");
  if (TARGET_32BIT && arm_arch4 && !(arm_arch8 && arm_arch_notm))
    {
      int coproc_level = 0x1;

      if (arm_arch5t)
	coproc_level |= 0x2;
      if (arm_arch5te)
	coproc_level |= 0x4;
      if (arm_arch6)
	coproc_level |= 0x8;

      builtin_define_with_int_value ("__ARM_FEATURE_COPROC", coproc_level);
    }

  def_or_undef_macro (pfile, "__ARM_FEATURE_CDE", TARGET_CDE);
  cpp_undef (pfile, "__ARM_FEATURE_CDE_COPROC");
  if (TARGET_CDE)
    builtin_define_with_int_value ("__ARM_FEATURE_CDE_COPROC",
				   arm_arch_cde_coproc);

  def_or_undef_macro (pfile, "__ARM_FEATURE_MATMUL_INT8", TARGET_I8MM);
  def_or_undef_macro (pfile, "__ARM_FEATURE_BF16_SCALAR_ARITHMETIC",
		      TARGET_BF16_FP);
  def_or_undef_macro (pfile, "__ARM_FEATURE_BF16_VECTOR_ARITHMETIC",
		      TARGET_BF16_SIMD);
  def_or_undef_macro (pfile, "__ARM_BF16_FORMAT_ALTERNATIVE",
		      TARGET_BF16_FP || TARGET_BF16_SIMD);
}

void
arm_cpu_cpp_builtins (struct cpp_reader * pfile)
{
  builtin_assert ("cpu=arm");
  builtin_assert ("machine=arm");

  arm_cpu_builtins (pfile);
}

/* Hook to validate the current #pragma GCC target and set the arch custom
   mode state.  If ARGS is NULL, then POP_TARGET is used to reset
   the options.  */

static bool
arm_pragma_target_parse (tree args, tree pop_target)
{
  tree prev_tree = target_option_current_node;
  tree cur_tree;
  struct cl_target_option *prev_opt;
  struct cl_target_option *cur_opt;

  if (! args)
    {
      cur_tree = ((pop_target) ? pop_target : target_option_default_node);
      cl_target_option_restore (&global_options, &global_options_set,
				TREE_TARGET_OPTION (cur_tree));
    }
  else
    {
      cur_tree = arm_valid_target_attribute_tree (args, &global_options,
						  &global_options_set);
      if (cur_tree == NULL_TREE)
	{
	  cl_target_option_restore (&global_options, &global_options_set,
				    TREE_TARGET_OPTION (prev_tree));
	  return false;
	}

      /* handle_pragma_pop_options and handle_pragma_reset_options will set
       target_option_current_node, but not handle_pragma_target.  */
      target_option_current_node = cur_tree;
      arm_configure_build_target (&arm_active_target,
				  TREE_TARGET_OPTION (cur_tree), false);
      arm_option_reconfigure_globals ();
    }

  /* Update macros if target_node changes. The global state will be restored
     by arm_set_current_function.  */
  prev_opt = TREE_TARGET_OPTION (prev_tree);
  cur_opt  = TREE_TARGET_OPTION (cur_tree);

  gcc_assert (prev_opt);
  gcc_assert (cur_opt);

  if (cur_opt != prev_opt)
    {
      /* For the definitions, ensure all newly defined macros are considered
	 as used for -Wunused-macros.  There is no point warning about the
	 compiler predefined macros.  */
      cpp_options *cpp_opts = cpp_get_options (parse_in);
      unsigned char saved_warn_unused_macros = cpp_opts->warn_unused_macros;

      cpp_opts->warn_unused_macros = 0;

      /* Update macros.  */
      gcc_assert (cur_opt->x_target_flags == target_flags);

      /* Don't warn for macros that have context sensitive values depending on
	 other attributes.
	 See warn_of_redefinition, reset after cpp_create_definition.  */
      tree acond_macro = get_identifier ("__ARM_NEON_FP");
      C_CPP_HASHNODE (acond_macro)->flags |= NODE_CONDITIONAL ;

      acond_macro = get_identifier ("__ARM_FP");
      C_CPP_HASHNODE (acond_macro)->flags |= NODE_CONDITIONAL;

      acond_macro = get_identifier ("__ARM_FEATURE_LDREX");
      C_CPP_HASHNODE (acond_macro)->flags |= NODE_CONDITIONAL;

      cpp_force_token_locations (parse_in, BUILTINS_LOCATION);
      arm_cpu_builtins (parse_in);
      cpp_stop_forcing_token_locations (parse_in);

      cpp_opts->warn_unused_macros = saved_warn_unused_macros;

      /* Make sure that target_reinit is called for next function, since
	 TREE_TARGET_OPTION might change with the #pragma even if there is
	 no target attribute attached to the function.  */
      arm_reset_previous_fndecl ();

      /* If going to the default mode, we restore the initial states.
	 if cur_tree is a new target, states will be saved/restored on a per
	 function basis in arm_set_current_function.  */
      if (cur_tree == target_option_default_node)
	save_restore_target_globals (cur_tree);
    }

  return true;
}

/* Register target pragmas.  We need to add the hook for parsing #pragma GCC
   option here rather than in arm.cc since it will pull in various preprocessor
   functions, and those are not present in languages like fortran without a
   preprocessor.  */

void
arm_register_target_pragmas (void)
{
  /* Update pragma hook to allow parsing #pragma GCC target.  */
  targetm.target_option.pragma_parse = arm_pragma_target_parse;

  targetm.resolve_overloaded_builtin = arm_resolve_overloaded_builtin;
  targetm.check_builtin_call = arm_check_builtin_call;

  c_register_pragma ("GCC", "arm", arm_pragma_arm);

#ifdef REGISTER_SUBTARGET_PRAGMAS
  REGISTER_SUBTARGET_PRAGMAS ();
#endif
}
