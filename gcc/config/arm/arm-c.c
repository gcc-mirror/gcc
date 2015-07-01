/* Copyright (C) 2007-2015 Free Software Foundation, Inc.

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
#include "tm_p.h"
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "tm_p.h"
#include "c-family/c-common.h"
#include "target.h"
#include "c-family/c-pragma.h"

/* Output C specific EABI object attributes.  These can not be done in
   arm.c because they require information from the C frontend.  */

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

void
arm_cpu_builtins (struct cpp_reader* pfile, int flags)
{
  def_or_undef_macro (pfile, "__ARM_FEATURE_DSP",
		      TARGET_DSP_MULTIPLY_P (flags));
  def_or_undef_macro (pfile, "__ARM_FEATURE_QBIT",
		      TARGET_ARM_QBIT_P (flags)); 
  def_or_undef_macro (pfile, "__ARM_FEATURE_SAT",
		      TARGET_ARM_SAT_P (flags));
  if (TARGET_CRYPTO)
    builtin_define ("__ARM_FEATURE_CRYPTO");
  if (unaligned_access)
    builtin_define ("__ARM_FEATURE_UNALIGNED");
  if (TARGET_CRC32)
    builtin_define ("__ARM_FEATURE_CRC32");

  def_or_undef_macro (pfile, "__ARM_32BIT_STATE", TARGET_32BIT_P (flags)); 

  if (TARGET_ARM_FEATURE_LDREX_P (flags))
    builtin_define_with_int_value ("__ARM_FEATURE_LDREX", 
				   TARGET_ARM_FEATURE_LDREX_P (flags));
  else
    cpp_undef (pfile, "__ARM_FEATURE_LDREX");

  def_or_undef_macro (pfile, "__ARM_FEATURE_CLZ",
		      ((TARGET_ARM_ARCH >= 5 && !TARGET_THUMB_P (flags))
		       || TARGET_ARM_ARCH_ISA_THUMB >=2));

  def_or_undef_macro (pfile, "__ARM_FEATURE_SIMD32", TARGET_INT_SIMD_P (flags));

  builtin_define_with_int_value ("__ARM_SIZEOF_MINIMAL_ENUM",
				 flag_short_enums ? 1 : 4);
  builtin_define_type_sizeof ("__ARM_SIZEOF_WCHAR_T", wchar_type_node);
  if (TARGET_ARM_ARCH_PROFILE)
    builtin_define_with_int_value ("__ARM_ARCH_PROFILE",
				   TARGET_ARM_ARCH_PROFILE);

  /* Define __arm__ even when in thumb mode, for
     consistency with armcc.  */
  builtin_define ("__arm__");
  if (TARGET_ARM_ARCH)
    builtin_define_with_int_value ("__ARM_ARCH", TARGET_ARM_ARCH);
  if (arm_arch_notm)
    builtin_define ("__ARM_ARCH_ISA_ARM");
  builtin_define ("__APCS_32__");

  def_or_undef_macro (pfile, "__thumb__", TARGET_THUMB_P (flags));
  def_or_undef_macro (pfile, "__thumb2__", TARGET_THUMB2_P (flags));
  if (TARGET_BIG_END)
    def_or_undef_macro (pfile, "__THUMBEB__", TARGET_THUMB_P (flags));
  else
    def_or_undef_macro (pfile, "__THUMBEL__", TARGET_THUMB_P (flags));

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

  if (TARGET_VFP)
    builtin_define ("__VFP_FP__");
	
  if (TARGET_ARM_FP)
    builtin_define_with_int_value ("__ARM_FP", TARGET_ARM_FP);
  if (arm_fp16_format == ARM_FP16_FORMAT_IEEE)
    builtin_define ("__ARM_FP16_FORMAT_IEEE");
  if (arm_fp16_format == ARM_FP16_FORMAT_ALTERNATIVE)
    builtin_define ("__ARM_FP16_FORMAT_ALTERNATIVE");
  if (TARGET_FMA)
    builtin_define ("__ARM_FEATURE_FMA");

  if (TARGET_NEON)
    {
      builtin_define ("__ARM_NEON__");
      builtin_define ("__ARM_NEON");
    }
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
  if (TARGET_AAPCS_BASED)
    {
      if (arm_pcs_default == ARM_PCS_AAPCS_VFP)
	builtin_define ("__ARM_PCS_VFP");
      else if (arm_pcs_default == ARM_PCS_AAPCS)
	builtin_define ("__ARM_PCS");
      builtin_define ("__ARM_EABI__");
    }

  def_or_undef_macro (pfile, "__ARM_ARCH_EXT_IDIV__", TARGET_IDIV_P (flags));
  def_or_undef_macro (pfile, "__ARM_FEATURE_IDIV", TARGET_IDIV_P (flags));

  def_or_undef_macro (pfile, "__ARM_ASM_SYNTAX_UNIFIED__", inline_asm_unified);
}

void
arm_cpu_cpp_builtins (struct cpp_reader * pfile)
{
  builtin_assert ("cpu=arm");
  builtin_assert ("machine=arm");

  arm_cpu_builtins (pfile, target_flags);
}

/* Hook to validate the current #pragma GCC target and set the arch custom
   mode state.  If ARGS is NULL, then POP_TARGET is used to reset
   the options.  */
static bool
arm_pragma_target_parse (tree args, tree pop_target)
{
  tree prev_tree = build_target_option_node (&global_options);
  tree cur_tree;
  struct cl_target_option *prev_opt;
  struct cl_target_option *cur_opt;

  if (! args)
    {
      cur_tree = ((pop_target) ? pop_target : target_option_default_node);
      cl_target_option_restore (&global_options,
				TREE_TARGET_OPTION (cur_tree));
    }
  else
    {
      cur_tree = arm_valid_target_attribute_tree (args, &global_options,
						  &global_options_set);
      if (cur_tree == NULL_TREE)
	{
	  cl_target_option_restore (&global_options,
				    TREE_TARGET_OPTION (prev_tree));
	  return false;
	}
    }

  target_option_current_node = cur_tree;
  arm_reset_previous_fndecl ();

  /* Figure out the previous mode.  */
  prev_opt  = TREE_TARGET_OPTION (prev_tree);
  cur_opt   = TREE_TARGET_OPTION (cur_tree);

  gcc_assert (prev_opt);
  gcc_assert (cur_opt);

  if (cur_opt->x_target_flags != prev_opt->x_target_flags)
    {
      /* For the definitions, ensure all newly defined macros are considered
	 as used for -Wunused-macros.  There is no point warning about the
	 compiler predefined macros.  */
      cpp_options *cpp_opts = cpp_get_options (parse_in);
      unsigned char saved_warn_unused_macros = cpp_opts->warn_unused_macros;
      cpp_opts->warn_unused_macros = 0;

      /* Update macros.  */
      arm_cpu_builtins (parse_in, cur_opt->x_target_flags);

      cpp_opts->warn_unused_macros = saved_warn_unused_macros;
    }

  return true;
}

/* Register target pragmas.  We need to add the hook for parsing #pragma GCC
   option here rather than in arm.c since it will pull in various preprocessor
   functions, and those are not present in languages like fortran without a
   preprocessor.  */

void
arm_register_target_pragmas (void)
{
  /* Update pragma hook to allow parsing #pragma GCC target.  */
  targetm.target_option.pragma_parse = arm_pragma_target_parse;

#ifdef REGISTER_SUBTARGET_PRAGMAS
  REGISTER_SUBTARGET_PRAGMAS ();
#endif
}
