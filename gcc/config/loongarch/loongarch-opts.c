/* Subroutines for loongarch-specific option handling.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.

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
#include "diagnostic-core.h"

#include "loongarch-opts.h"

const char* loongarch_isa_int_strings[] = {
    /* ISA_LA64 */  "la64",
};

const char* loongarch_isa_float_strings[] = {
    /* ISA_SOFT_FLOAT */     "none",
    /* ISA_SINGLE_FLOAT */   "single",
    /* ISA_DOUBLE_FLOAT */   "double",
};

const char* loongarch_abi_int_strings[] = {
    /* ABI_LP64 */  "lp64",
};

const char* loongarch_abi_float_strings[] = {
    /* ABI_SOFT_FLOAT */     "soft",
    /* ABI_SINGLE_FLOAT */   "single",
    /* ABI_DOUBLE_FLOAT */   "double",
};

/* Handle combinations of -m machine option values.
   (see loongarch.opt and loongarch-opts.h) */

void
loongarch_handle_m_option_combinations (
  int* cpu_arch, int* cpu_tune, int* isa_int, int* isa_float,
  int* abi_int, int* abi_float, int* native_cpu_type,
  int* ext_fix_lns3_llsc)
{
  /* Rules for handling machine-specific options:

     1. "-m" options override configure-time default "--with" options,
     which provides the build-time default value of machine-specific settings.

     2. The overall goal of handling "-m" options is to obtain a basic
     set of target ISA/ABI requirements, which consists of five components:
     | integer ISA | floating-point ISA | LoongArch ISA extensions |
     | integer ABI | floating-point ABI |

     These components may not be combined in arbitrary ways, for example,
     we can't have LP64 ABI on LA32 instruction set. They are designed
     as separate components for flexibility and clearity of backend coding.

     Handling of conflicts and overriding between "-m" options
     given at once is the job of this function.

     3. Aside from its own effects (custom preprocessing macros etc.),
     "-march=CPU" also implies a basic target ISA/ABI set
     and "-mtune=CPU", which can be overridden by other "-m" options.

     4. For now, floating-point ISA decides the fp ABI.
     */

  /* 1. Compute march / mtune */
  int cpu_arch_was_absent = LARCH_OPT_ABSENT(*cpu_arch);

  if (LARCH_OPT_ABSENT(*cpu_arch))
    /* Using configure-time default.  */
    *cpu_arch = DEFAULT_CPU_ARCH;

  if (LARCH_OPT_ABSENT(*cpu_tune))
    /* Try inferring -mtune from -march.  */
    *cpu_tune = *cpu_arch;


#ifdef __loongarch__
  /* For native compilers, gather local CPU information
     and fill the "CPU_NATIVE" index of arrays defined in
     cpu/loongarch-cpu.c.  */

  cache_cpucfg();

  int cpu_detected = fill_native_cpu_config();
  if (cpu_detected == -1)
    {
      error ("unknown processor ID %<0x%x%> detected, do not use %qs",
	     get_native_prid(), "-march=native");

      if (native_cpu_type != NULL)
	*native_cpu_type = M_OPTION_NOT_SEEN;
    }

  else if (native_cpu_type != NULL)
    /* Record detected PRID in *native_cpu_type */
    *native_cpu_type = cpu_detected;

#else
  if (*cpu_arch == CPU_NATIVE)
    {
      *cpu_arch = DEFAULT_CPU_ARCH;
      error ("%<-march=%s%> does not work on a cross compiler.",
	     loongarch_cpu_strings[CPU_NATIVE]);
    }

  else if (*cpu_tune == CPU_NATIVE)
    {
      *cpu_tune = DEFAULT_CPU_ARCH;
      error ("%<-mtune=%s%> does not work on a cross compiler.",
	     loongarch_cpu_strings[CPU_NATIVE]);
    }

  if (native_cpu_type != NULL)
    *native_cpu_type = *cpu_arch;
#endif


  /* 2. Compute ISA Extensions */

#define SET_EXT_FLAG(name)                                   \
  if (ext_##name != NULL)                                    \
  if (LARCH_OPT_ABSENT (*ext_##name))                        \
    {                                                        \
      if (!LARCH_OPT_ABSENT ( DEFAULT_EXT_##name ))          \
	*ext_##name = DEFAULT_EXT_##name ;                   \
      else                                                   \
	*ext_##name                                          \
	  = loongarch_cpu_default_config[*cpu_arch].ext.name ; \
    }

  SET_EXT_FLAG(fix_lns3_llsc)

  /* 3. Compute integer ISA */
  int int_isa_was_absent = 0;
  if (LARCH_OPT_ABSENT(*isa_int))
    {
      int_isa_was_absent = 1;

      /* Try inferring int isa from int abi.  */
      if (!LARCH_OPT_ABSENT(*abi_int))
	switch (*abi_int)
	  {
	  case ABI_LP64:
	    *isa_int = ISA_LA64;
	    break;

	  default:
	    gcc_unreachable();
	  }

      /* Try inferring int abi from "-march" option.  */
      else if (!cpu_arch_was_absent)
	*isa_int
	  = loongarch_cpu_default_config[*cpu_arch].isa_int;

      /* If "-march" wasn't present, set *isa_int to configure-time default.  */
      else if (DEFAULT_ISA_INT != M_OPTION_NOT_SEEN)
	*isa_int = DEFAULT_ISA_INT;

      /* If there's not a configure-time default for integer ISA,
       * infer it from the configure-time default of "-march".  */
      else
	*isa_int = loongarch_cpu_default_config[*cpu_arch].isa_int;
    }

  /* 4. Compute integer ABI */
  if (LARCH_OPT_ABSENT(*abi_int))
    {
      /* Try inferring int abi from -march first
       * (or loongarch_cpu_default_config[].abi_int
       * will not be effective under any circumstance).  */

      if (int_isa_was_absent)
	if (!cpu_arch_was_absent)
	  *abi_int
	    = loongarch_cpu_default_config[*cpu_arch].abi_int;

	else if (DEFAULT_ABI_INT != M_OPTION_NOT_SEEN)
	  /* Fall back to configure-time default, if there is one.  */
	  *abi_int = DEFAULT_ABI_INT;

	else
	  /* If there's no configure-time default for integer ABI,
	   * infer it from the configure-time default of "-march".  */
	  *abi_int
	    = loongarch_cpu_default_config[*cpu_arch].abi_int;

      else
	/* Try inferring int ABI from int ISA.  */
	switch (*isa_int)
	  {
	  case ISA_LA64:
	    *abi_int = ABI_LP64;
	    break;

	  default:
	    gcc_unreachable();
	  }
    }

  /* 5. Check integer ABI-ISA for conflicts */
  switch(*isa_int)
    {
    case ISA_LA64:
      if (*abi_int != ABI_LP64) goto error_int_abi;
      break;

    default:
      error_int_abi:
      error ("%qs ABI is incompatible with %qs ISA",
	     loongarch_abi_int_strings[*abi_int],
	     loongarch_isa_int_strings[*isa_int]
	    );
    }

  /* 6. Compute floating-point ISA */
  int float_isa_was_absent = 0;
  if (LARCH_OPT_ABSENT(*isa_float))
    {
      float_isa_was_absent = 1;

      /* Try inferring fp isa from fp abi.  */
      if (!LARCH_OPT_ABSENT(*abi_float))
	switch (*abi_float)
	  {
	  case ABI_SOFT_FLOAT:
	    *isa_float = ISA_SOFT_FLOAT;
	    break;

	  case ABI_SINGLE_FLOAT:
	    *isa_float = ISA_SINGLE_FLOAT;
	    break;

	  case ABI_DOUBLE_FLOAT:
	    *isa_float = ISA_DOUBLE_FLOAT;
	    break;

	  default:
	    gcc_unreachable();
	  }

      /* Try inferring fp abi from "-march" option.  */
      else if (!cpu_arch_was_absent)
	*isa_float = loongarch_cpu_default_config[*cpu_arch].isa_float;

      /* If "-march" were't present, set *isa_float to config-time default.  */
      else if (DEFAULT_ISA_FLOAT != M_OPTION_NOT_SEEN)
	*isa_float = DEFAULT_ISA_FLOAT;

      /* If there's not a configure-time default for floating-point ISA,
       * infer it from the configure-time default of "-march".  */
      else
	*isa_float = loongarch_cpu_default_config[*cpu_arch].isa_float;
    }

  /* 7. Compute floating-point ABI */
  if (LARCH_OPT_ABSENT(*abi_float))
    {
      /* Try inferring fp abi from cpu_arch first
	 (or loongarch_cpu_default_config[].abi_float
	 will not be effective under any circumstance).
	 */
      if (float_isa_was_absent)

	if (!cpu_arch_was_absent)
	  *abi_float
	    = loongarch_cpu_default_config[*cpu_arch].abi_float;

	else if (DEFAULT_ABI_FLOAT != M_OPTION_NOT_SEEN)
	  /* Fall back to configure-time default, if there is one.  */
	  *abi_float = DEFAULT_ABI_FLOAT;

	else
	  /* If there's no configure-time default for floating-point ABI,
	   * infer it from the configure-time default of "-march".  */
	  *abi_float
	    = loongarch_cpu_default_config[*cpu_arch].abi_float;

      else
	/* Try inferring fp abi from fp isa.  */
	switch (*isa_float)
	  {
	  case ISA_SOFT_FLOAT:
	    *abi_float = ABI_SOFT_FLOAT;
	    break;

	  case ISA_SINGLE_FLOAT:
	    *abi_float = ABI_SINGLE_FLOAT;
	    break;

	  case ISA_DOUBLE_FLOAT:
	    *abi_float = ABI_DOUBLE_FLOAT;
	    break;

	  default:
	    gcc_unreachable();
	  }
    }
  /* 8. Check floating-point ABI-ISA for conflicts */
  else if (!float_isa_was_absent)
    {
      switch(*isa_float)
	{
	case ISA_SOFT_FLOAT:
	  if (*abi_float != ABI_SOFT_FLOAT) goto error_float_abi;
	  break;

	case ISA_SINGLE_FLOAT:
	  if (*abi_float != ABI_SINGLE_FLOAT) goto error_float_abi;
	  break;

	case ISA_DOUBLE_FLOAT:
	  if (*abi_float != ABI_DOUBLE_FLOAT) goto error_float_abi;
	  break;

	default:
	  error_float_abi:
	  error ("%<%s-float%> ABI is incompatible with %qs fpu",
		 loongarch_abi_float_strings[*abi_float],
		 loongarch_isa_float_strings[*isa_float]
		);
	}
    }
}
