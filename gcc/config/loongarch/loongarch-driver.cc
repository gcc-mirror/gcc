/* Subroutines for the gcc driver.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.
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
#include "opts.h"

#include "loongarch-opts.h"
#include "loongarch-driver.h"

/* This flag is set to 1 if we believe that the user might be avoiding
   linking (implicitly) against something from the startfile search paths.  */
static int no_link = 0;

/* Use the public obstack from the gcc driver (defined in gcc.c).
   This is for allocating space for the returned string.  */
extern struct obstack opts_obstack;

const char*
la_driver_init (int argc ATTRIBUTE_UNUSED, const char **argv ATTRIBUTE_UNUSED)
{
  /* Initialize all fields of la_target.  */
  loongarch_init_target (&la_target, M_OPT_UNSET, M_OPT_UNSET, M_OPT_UNSET,
			 M_OPT_UNSET, M_OPT_UNSET, M_OPT_UNSET, M_OPT_UNSET,
			 M_OPT_UNSET, 0, 0);
  return "";
}

const char*
driver_set_no_link (int argc ATTRIBUTE_UNUSED,
		    const char **argv ATTRIBUTE_UNUSED)
{
  no_link = 1;
  return "";
}

const char*
driver_set_m_parm (int argc, const char **argv)
{
  gcc_assert (argc == 2);

#define LARCH_DRIVER_PARSE_PARM(OPT_IDX, NAME, OPTSTR_LIST, \
				OPT_IDX_LO, OPT_IDX_HI)	    \
  if (strcmp (argv[0], OPTSTR_##NAME) == 0)		    \
    for (int i = (OPT_IDX_LO); i < (OPT_IDX_HI); i++)	    \
    {							    \
      if ((OPTSTR_LIST)[i] != 0)			    \
	if (strcmp (argv[1], (OPTSTR_LIST)[i]) == 0)	    \
	  {						    \
	    (OPT_IDX) = i;				    \
	    return 0;					    \
	  }						    \
    }

  LARCH_DRIVER_PARSE_PARM (la_target.abi.base, ABI_BASE, \
			   loongarch_abi_base_strings, 0, N_ABI_BASE_TYPES)

  LARCH_DRIVER_PARSE_PARM (la_target.isa.fpu, ISA_EXT_FPU, \
			   loongarch_isa_ext_strings, 0, N_ISA_EXT_FPU_TYPES)

  LARCH_DRIVER_PARSE_PARM (la_target.isa.simd, ISA_EXT_SIMD, \
			   loongarch_isa_ext_strings, 0, N_ISA_EXT_TYPES)

  LARCH_DRIVER_PARSE_PARM (la_target.cpu_arch, ARCH, \
			   loongarch_arch_strings, 0, N_ARCH_TYPES)

  LARCH_DRIVER_PARSE_PARM (la_target.cpu_tune, TUNE, \
			   loongarch_tune_strings, 0, N_TUNE_TYPES)

  LARCH_DRIVER_PARSE_PARM (la_target.cmodel, CMODEL, \
			   loongarch_cmodel_strings, 0, N_CMODEL_TYPES)

  gcc_unreachable ();
}

static void
driver_record_deferred_opts (struct loongarch_flags *flags)
{
  unsigned int i;
  cl_deferred_option *opt;
  vec<cl_deferred_option> *v = (vec<cl_deferred_option> *) la_deferred_options;

  gcc_assert (flags);

  /* Initialize flags */
  flags->flt = M_OPT_UNSET;
  flags->flt_str = NULL;
  flags->sx[0] = flags->sx[1] = 0;

  int sx_flag_idx = 0;

  if (v)
    FOR_EACH_VEC_ELT (*v, i, opt)
      {
	switch (opt->opt_index)
	  {
	  case OPT_mlsx:
	    flags->sx[sx_flag_idx++] = ISA_EXT_SIMD_LSX
	      * (opt->value ? 1 : -1);
	    break;

	  case OPT_mlasx:
	    flags->sx[sx_flag_idx++] = ISA_EXT_SIMD_LASX
	      * (opt->value ? 1 : -1);
	    break;

	  case OPT_msoft_float:
	    flags->flt = ISA_EXT_NONE;
	    flags->flt_str = OPTSTR_SOFT_FLOAT;
	    break;

	  case OPT_msingle_float:
	    flags->flt = ISA_EXT_FPU32;
	    flags->flt_str = OPTSTR_SINGLE_FLOAT;
	    break;

	  case OPT_mdouble_float:
	    flags->flt = ISA_EXT_FPU64;
	    flags->flt_str = OPTSTR_DOUBLE_FLOAT;
	    break;

	  default:
	    gcc_unreachable ();
	  }
	gcc_assert (sx_flag_idx <= 2);
      }
}

const char*
driver_get_normalized_m_opts (int argc, const char **argv ATTRIBUTE_UNUSED)
{
  if (argc != 0)
    return " %eget_normalized_m_opts requires no argument.\n";

  struct loongarch_flags flags;
  driver_record_deferred_opts (&flags);
  loongarch_config_target (&la_target, &flags, !no_link);

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

#undef CLEAR_FLAG
#define CLEAR_FLAG(NAME) \
  APPEND_LTR (" %<m" NAME " %<mno-" NAME)

  CLEAR_FLAG (STR_ISA_EXT_LSX);
  CLEAR_FLAG (STR_ISA_EXT_LASX);
  CLEAR_FLAG (OPTSTR_SOFT_FLOAT);
  CLEAR_FLAG (OPTSTR_SINGLE_FLOAT);
  CLEAR_FLAG (OPTSTR_DOUBLE_FLOAT);

  APPEND_OPT (ABI_BASE);
  APPEND_VAL (loongarch_abi_base_strings[la_target.abi.base]);

  APPEND_OPT (ARCH);
  APPEND_VAL (loongarch_arch_strings[la_target.cpu_arch]);

  APPEND_OPT (ISA_EXT_FPU);
  APPEND_VAL (loongarch_isa_ext_strings[la_target.isa.fpu]);

  APPEND_OPT (ISA_EXT_SIMD);
  APPEND_VAL (loongarch_isa_ext_strings[la_target.isa.simd]);

  APPEND_OPT (CMODEL);
  APPEND_VAL (loongarch_cmodel_strings[la_target.cmodel]);

  APPEND_OPT (TUNE);
  APPEND_VAL (loongarch_tune_strings[la_target.cpu_tune]);

  obstack_1grow (&opts_obstack, '\0');

  return XOBFINISH (&opts_obstack, const char *);
}
