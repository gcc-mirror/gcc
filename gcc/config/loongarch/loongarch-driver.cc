/* Subroutines for the gcc driver.
   Copyright (C) 2021-2023 Free Software Foundation, Inc.
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
#include "tm.h"
#include "obstack.h"
#include "diagnostic-core.h"

#include "loongarch-opts.h"
#include "loongarch-driver.h"

static int
  opt_arch_driver = M_OPTION_NOT_SEEN,
  opt_tune_driver = M_OPTION_NOT_SEEN,
  opt_fpu_driver = M_OPTION_NOT_SEEN,
  opt_abi_base_driver = M_OPTION_NOT_SEEN,
  opt_abi_ext_driver = M_OPTION_NOT_SEEN,
  opt_cmodel_driver = M_OPTION_NOT_SEEN;

int opt_switches = 0;

/* This flag is set to 1 if we believe that the user might be avoiding
   linking (implicitly) against something from the startfile search paths.  */
static int no_link = 0;

#define LARCH_DRIVER_SET_M_FLAG(OPTS_ARRAY, N_OPTS, FLAG, STR)	\
  for (int i = 0; i < (N_OPTS); i++)				\
  {								\
    if ((OPTS_ARRAY)[i] != 0)					\
      if (strcmp ((STR), (OPTS_ARRAY)[i]) == 0)			\
	(FLAG) = i;						\
  }

/* Use the public obstack from the gcc driver (defined in gcc.c).
   This is for allocating space for the returned string.  */
extern struct obstack opts_obstack;

#define APPEND_LTR(S)				      \
  obstack_grow (&opts_obstack, (const void*) (S),     \
		sizeof ((S)) / sizeof (char) -1)

#define APPEND_VAL(S) \
  obstack_grow (&opts_obstack, (const void*) (S), strlen ((S)))


const char*
driver_set_m_flag (int argc, const char **argv)
{
  int parm_off = 0;

  if (argc != 1)
    return "%eset_m_flag requires exactly 1 argument.";

#undef PARM
#define PARM (argv[0] + parm_off)

/* Note: sizeof (OPTSTR_##NAME) equals the length of "<option>=".  */
#undef MATCH_OPT
#define MATCH_OPT(NAME) \
  (strncmp (argv[0], OPTSTR_##NAME "=", \
	    (parm_off = sizeof (OPTSTR_##NAME))) == 0)

  if (strcmp (argv[0], "no_link") == 0)
    {
      no_link = 1;
    }
  else if (MATCH_OPT (ABI_BASE))
    {
      LARCH_DRIVER_SET_M_FLAG (
	loongarch_abi_base_strings, N_ABI_BASE_TYPES,
	opt_abi_base_driver, PARM)
    }
  else if (MATCH_OPT (ISA_EXT_FPU))
    {
      LARCH_DRIVER_SET_M_FLAG (loongarch_isa_ext_strings, N_ISA_EXT_FPU_TYPES,
			       opt_fpu_driver, PARM)
    }
  else if (MATCH_OPT (ARCH))
    {
      LARCH_DRIVER_SET_M_FLAG (loongarch_cpu_strings, N_ARCH_TYPES,
			       opt_arch_driver, PARM)
    }
  else if (MATCH_OPT (TUNE))
    {
      LARCH_DRIVER_SET_M_FLAG (loongarch_cpu_strings, N_TUNE_TYPES,
			       opt_tune_driver, PARM)
    }
  else if (MATCH_OPT (CMODEL))
    {
      LARCH_DRIVER_SET_M_FLAG (loongarch_cmodel_strings, N_CMODEL_TYPES,
			       opt_cmodel_driver, PARM)
    }
  else /* switches */
    {
      int switch_idx = M_OPTION_NOT_SEEN;

      LARCH_DRIVER_SET_M_FLAG (loongarch_switch_strings, N_SWITCH_TYPES,
			       switch_idx, argv[0])

      if (switch_idx != M_OPTION_NOT_SEEN)
	opt_switches |= loongarch_switch_mask[switch_idx];
    }
  return "";
}

const char*
driver_get_normalized_m_opts (int argc, const char **argv)
{
  if (argc != 0)
    {
      (void) argv;  /* To make compiler shut up about unused argument.  */
      return " %eget_normalized_m_opts requires no argument.\n";
    }

  loongarch_config_target (& la_target,
			   opt_switches,
			   opt_arch_driver,
			   opt_tune_driver,
			   opt_fpu_driver,
			   opt_abi_base_driver,
			   opt_abi_ext_driver,
			   opt_cmodel_driver,
			   !no_link /* follow_multilib_list */);

  /* Output normalized option strings.  */
  obstack_blank (&opts_obstack, 0);

#undef APPEND_LTR
#define APPEND_LTR(S) \
  obstack_grow (&opts_obstack, (const void*) (S), \
		sizeof ((S)) / sizeof (char) -1)

#undef APPEND_VAL
#define APPEND_VAL(S) \
  obstack_grow (&opts_obstack, (const void*) (S), strlen ((S)))

#undef APPEND_OPT
#define APPEND_OPT(NAME) \
   APPEND_LTR (" %<m" OPTSTR_##NAME "=* " \
	       " -m" OPTSTR_##NAME "=")

  for (int i = 0; i < N_SWITCH_TYPES; i++)
    {
      APPEND_LTR (" %<m");
      APPEND_VAL (loongarch_switch_strings[i]);
    }

  APPEND_OPT (ABI_BASE);
  APPEND_VAL (loongarch_abi_base_strings[la_target.abi.base]);

  APPEND_OPT (ARCH);
  APPEND_VAL (loongarch_cpu_strings[la_target.cpu_arch]);

  APPEND_OPT (ISA_EXT_FPU);
  APPEND_VAL (loongarch_isa_ext_strings[la_target.isa.fpu]);

  APPEND_OPT (CMODEL);
  APPEND_VAL (loongarch_cmodel_strings[la_target.cmodel]);

  APPEND_OPT (TUNE);
  APPEND_VAL (loongarch_cpu_strings[la_target.cpu_tune]);

  obstack_1grow (&opts_obstack, '\0');

  return XOBFINISH (&opts_obstack, const char *);
}
