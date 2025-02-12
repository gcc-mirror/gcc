/* LoongArch-specific code for C family languages.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.
   Contributed by Loongson Ltd.

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
#include "target.h"
#include "tm.h"
#include "c-family/c-common.h"
#include "cpplib.h"
#include "c-family/c-pragma.h"
#include "tm_p.h"

#define preprocessing_asm_p() (cpp_get_options (pfile)->lang == CLK_ASM)
#define builtin_define(TXT) cpp_define (pfile, TXT)
#define builtin_undef(TXT) cpp_undef (pfile, TXT)
#define builtin_assert(TXT) cpp_assert (pfile, TXT)

static void
loongarch_def_or_undef (bool def_p, const char *macro, cpp_reader *pfile)
{
  if (def_p)
    cpp_define (pfile, macro);
  else
    cpp_undef (pfile, macro);
}

static void
loongarch_define_unconditional_macros (cpp_reader *pfile)
{
  builtin_define ("__loongarch__");

  /* Base architecture / ABI.  */
  if (TARGET_64BIT)
    {
      builtin_define ("__loongarch_grlen=64");
      builtin_define ("__loongarch64");
    }

  if (TARGET_ABI_LP64)
    {
      builtin_define ("_ABILP64=3");
      builtin_define ("_LOONGARCH_SIM=_ABILP64");
      builtin_define ("__loongarch_lp64");
    }

  /* Add support for FLOAT128_TYPE on the LoongArch architecture.  */
  builtin_define ("__FLOAT128_TYPE__");

  /* Map the old _Float128 'q' builtins into the new 'f128' builtins.  */
  builtin_define ("__builtin_fabsq=__builtin_fabsf128");
  builtin_define ("__builtin_copysignq=__builtin_copysignf128");
  builtin_define ("__builtin_nanq=__builtin_nanf128");
  builtin_define ("__builtin_nansq=__builtin_nansf128");
  builtin_define ("__builtin_infq=__builtin_inff128");
  builtin_define ("__builtin_huge_valq=__builtin_huge_valf128");

  /* Native Data Sizes.  */
  builtin_define_with_int_value ("_LOONGARCH_SZINT", INT_TYPE_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_SZLONG", LONG_TYPE_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_SZPTR", POINTER_SIZE);
  builtin_define_with_int_value ("_LOONGARCH_FPSET", 32);
  builtin_define_with_int_value ("_LOONGARCH_SPFPSET", 32);
}

static void
loongarch_update_cpp_builtins (cpp_reader *pfile)
{
  /* Since the macros in this function might be redefined, it's necessary to
     undef them first.*/
  builtin_undef ("__loongarch_arch");
  builtin_define_with_value ("__loongarch_arch",
			     loongarch_arch_strings[la_target.cpu_arch], 1);

  builtin_undef ("__loongarch_tune");
  builtin_define_with_value ("__loongarch_tune",
			     loongarch_tune_strings[la_target.cpu_tune], 1);

  builtin_undef ("_LOONGARCH_ARCH");
  builtin_define_with_value ("_LOONGARCH_ARCH",
			     loongarch_arch_strings[la_target.cpu_arch], 1);

  builtin_undef ("_LOONGARCH_TUNE");
  builtin_define_with_value ("_LOONGARCH_TUNE",
			     loongarch_tune_strings[la_target.cpu_tune], 1);

  builtin_undef ("__loongarch_double_float");
  builtin_undef ("__loongarch_single_float");
  /* These defines reflect the ABI in use, not whether the
     FPU is directly accessible.  */
  if (TARGET_DOUBLE_FLOAT_ABI)
    builtin_define ("__loongarch_double_float=1");
  else if (TARGET_SINGLE_FLOAT_ABI)
    builtin_define ("__loongarch_single_float=1");

  builtin_undef ("__loongarch_soft_float");
  builtin_undef ("__loongarch_hard_float");
  if (TARGET_DOUBLE_FLOAT_ABI || TARGET_SINGLE_FLOAT_ABI)
    builtin_define ("__loongarch_hard_float=1");
  else
    builtin_define ("__loongarch_soft_float=1");


  /* ISA Extensions.  */
  builtin_undef ("__loongarch_frlen");
  if (TARGET_DOUBLE_FLOAT)
    builtin_define ("__loongarch_frlen=64");
  else if (TARGET_SINGLE_FLOAT)
    builtin_define ("__loongarch_frlen=32");
  else
    builtin_define ("__loongarch_frlen=0");

  loongarch_def_or_undef (ISA_HAS_LSX, "__loongarch_simd", pfile);
  loongarch_def_or_undef (ISA_HAS_LSX, "__loongarch_sx", pfile);
  loongarch_def_or_undef (ISA_HAS_LASX, "__loongarch_asx", pfile);

  builtin_undef ("__loongarch_simd_width");
  if (ISA_HAS_LSX)
    {
      if (ISA_HAS_LASX)
	builtin_define ("__loongarch_simd_width=256");
      else
	builtin_define ("__loongarch_simd_width=128");
    }

  /* ISA evolution features */
  int max_v_major = 1, max_v_minor = 0;

  for (int i = 0; i < N_EVO_FEATURES; i++)
    {
      builtin_undef (la_evo_macro_name[i]);

      if (la_target.isa.evolution & la_evo_feature_masks[i]
	  && (la_evo_feature_masks[i] != OPTION_MASK_ISA_FRECIPE
	      || TARGET_HARD_FLOAT))
	{
	  builtin_define (la_evo_macro_name[i]);

	  int major = la_evo_version_major[i],
	  minor = la_evo_version_minor[i];

	  max_v_major = major > max_v_major ? major : max_v_major;
	  max_v_minor = major == max_v_major
	    ? (minor > max_v_minor ? minor : max_v_minor) : max_v_minor;
	}
    }

  /* Find the minimum ISA version required to run the target program.  */
  builtin_undef ("__loongarch_version_major");
  builtin_undef ("__loongarch_version_minor");
  if (!(max_v_major == 1 && max_v_minor <= 1 && ISA_HAS_LASX))
    {
      builtin_define_with_int_value ("__loongarch_version_major", max_v_major);
      builtin_define_with_int_value ("__loongarch_version_minor", max_v_minor);
    }
}

void
loongarch_cpu_cpp_builtins (cpp_reader *pfile)
{
  builtin_assert ("machine=loongarch");
  builtin_assert ("cpu=loongarch");

  loongarch_define_unconditional_macros (pfile);
  loongarch_update_cpp_builtins (pfile);
}

/* Hook to validate the current #pragma GCC target and set the state, and
   update the macros based on what was changed.  If ARGS is NULL, then
   POP_TARGET is used to reset the options.  */

static bool
loongarch_pragma_target_parse (tree args, tree pop_target)
{
  /* If args is not NULL then process it and setup the target-specific
     information that it specifies.  */
  if (args)
    {
      if (!loongarch_process_target_attr (args, NULL))
	return false;

      loongarch_option_override_internal (&la_target,
					  &global_options,
					  &global_options_set);
    }

  /* args is NULL, restore to the state described in pop_target.  */
  else
    {
      pop_target = pop_target ? pop_target : target_option_default_node;
      cl_target_option_restore (&global_options, &global_options_set,
				TREE_TARGET_OPTION (pop_target));
    }

  target_option_current_node
    = build_target_option_node (&global_options, &global_options_set);

  loongarch_reset_previous_fndecl ();

  /* For the definitions, ensure all newly defined macros are considered
     as used for -Wunused-macros.  There is no point warning about the
     compiler predefined macros.  */
  cpp_options *cpp_opts = cpp_get_options (parse_in);
  unsigned char saved_warn_unused_macros = cpp_opts->warn_unused_macros;
  cpp_opts->warn_unused_macros = 0;

  cpp_force_token_locations (parse_in, BUILTINS_LOCATION);
  loongarch_update_cpp_builtins (parse_in);
  cpp_stop_forcing_token_locations (parse_in);

  cpp_opts->warn_unused_macros = saved_warn_unused_macros;

  /* If we're popping or reseting make sure to update the globals so that
     the optab availability predicates get recomputed.  */
  if (pop_target)
    loongarch_save_restore_target_globals (pop_target);

  return true;
}

/* Implement REGISTER_TARGET_PRAGMAS.  */

void
loongarch_register_pragmas (void)
{
  /* Update pragma hook to allow parsing #pragma GCC target.  */
  targetm.target_option.pragma_parse = loongarch_pragma_target_parse;
}
