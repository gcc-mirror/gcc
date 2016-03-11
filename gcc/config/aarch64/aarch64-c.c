/* Target-specific code for C family languages.
   Copyright (C) 2015-2016 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "input.h"
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

  aarch64_def_or_undef (TARGET_SIMD, "__ARM_FEATURE_NUMERIC_MAXMIN", pfile);
  aarch64_def_or_undef (TARGET_SIMD, "__ARM_NEON", pfile);


  aarch64_def_or_undef (TARGET_CRC32, "__ARM_FEATURE_CRC32", pfile);

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
      if (!aarch64_process_target_attr (args, "pragma"))
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

/* Implement REGISTER_TARGET_PRAGMAS.  */

void
aarch64_register_pragmas (void)
{
  /* Update pragma hook to allow parsing #pragma GCC target.  */
  targetm.target_option.pragma_parse = aarch64_pragma_target_parse;
}
