/* Definitions for LoongArch CPU properties.
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
#include "diagnostic-core.h"

#include "loongarch-opts.h"
#include "loongarch-cpu.h"
#include "loongarch-str.h"

/* Native CPU detection with "cpucfg" */
#define N_CPUCFG_WORDS 0x15
static uint32_t cpucfg_cache[N_CPUCFG_WORDS] = { 0 };
static const int cpucfg_useful_idx[] = {0, 1, 2, 16, 17, 18, 19};

static uint32_t
read_cpucfg_word (int wordno)
{
  /* To make cross-compiler shut up.  */
  (void) wordno;
  uint32_t ret = 0;

  #ifdef __loongarch__
  __asm__ __volatile__ ("cpucfg %0,%1\n\t"
			:"=r"(ret)
			:"r"(wordno)
			:);
  #endif

  return ret;
}

void
cache_cpucfg (void)
{
  for (unsigned int i = 0; i < sizeof (cpucfg_useful_idx) / sizeof (int); i++)
    {
      cpucfg_cache[cpucfg_useful_idx[i]]
	= read_cpucfg_word (cpucfg_useful_idx[i]);
    }
}

uint32_t
get_native_prid (void)
{
  /* Fill loongarch_cpu_default_config[CPU_NATIVE] with cpucfg data,
     see "Loongson Architecture Reference Manual"
     (Volume 1, Section 2.2.10.5) */
  return cpucfg_cache[0];
}

const char*
get_native_prid_str (void)
{
  static char prid_str[9];
  sprintf (prid_str, "%08x", cpucfg_cache[0]);
  return (const char*) prid_str;
}

/* Fill property tables for CPU_NATIVE.  */
unsigned int
fill_native_cpu_config (int p_arch_native, int p_tune_native)
{
  int ret_cpu_type;

  /* Nothing needs to be done unless "-march/tune=native"
     is given or implied.  */
  if (!(p_arch_native || p_tune_native))
    return CPU_NATIVE;

  /* Fill cpucfg_cache with the "cpucfg" instruction.  */
  cache_cpucfg ();


  /* Fill: loongarch_cpu_default_isa[CPU_NATIVE].base
     With: base architecture (ARCH)
     At:   cpucfg_words[1][1:0] */

  #define NATIVE_BASE_ISA (loongarch_cpu_default_isa[CPU_NATIVE].base)
  switch (cpucfg_cache[1] & 0x3)
    {
      case 0x02:
	NATIVE_BASE_ISA = ISA_BASE_LA64V100;
	break;

      default:
	if (p_arch_native)
	  fatal_error (UNKNOWN_LOCATION,
		       "unknown base architecture %<0x%x%>, %qs failed",
		       (unsigned int) (cpucfg_cache[1] & 0x3),
		       "-m" OPTSTR_ARCH "=" STR_CPU_NATIVE);
    }

  /* Fill: loongarch_cpu_default_isa[CPU_NATIVE].fpu
     With: FPU type (FP, FP_SP, FP_DP)
     At:   cpucfg_words[2][2:0] */

  #define NATIVE_FPU (loongarch_cpu_default_isa[CPU_NATIVE].fpu)
  switch (cpucfg_cache[2] & 0x7)
    {
      case 0x07:
	NATIVE_FPU = ISA_EXT_FPU64;
	break;

      case 0x03:
	NATIVE_FPU = ISA_EXT_FPU32;
	break;

      case 0x00:
	NATIVE_FPU = ISA_EXT_NOFPU;
	break;

      default:
	if (p_arch_native)
	  fatal_error (UNKNOWN_LOCATION,
		       "unknown FPU type %<0x%x%>, %qs failed",
		       (unsigned int) (cpucfg_cache[2] & 0x7),
		       "-m" OPTSTR_ARCH "=" STR_CPU_NATIVE);
    }

  /* Fill: loongarch_cpu_cache[CPU_NATIVE]
     With: cache size info
     At:   cpucfg_words[16:20][31:0] */

  int l1d_present = 0, l1u_present = 0;
  int l2d_present = 0;
  uint32_t l1_szword, l2_szword;

  l1u_present |= cpucfg_cache[16] & 3;	      /* bit[1:0]: unified l1 cache */
  l1d_present |= cpucfg_cache[16] & 4;	      /* bit[2:2]: l1 dcache */
  l1_szword = l1d_present ? 18 : (l1u_present ? 17 : 0);
  l1_szword = l1_szword ? cpucfg_cache[l1_szword]: 0;

  l2d_present |= cpucfg_cache[16] & 24;	      /* bit[4:3]: unified l2 cache */
  l2d_present |= cpucfg_cache[16] & 128;      /* bit[7:7]: l2 dcache */
  l2_szword = l2d_present ? cpucfg_cache[19]: 0;

  loongarch_cpu_cache[CPU_NATIVE].l1d_line_size
    = 1 << ((l1_szword & 0x7f000000) >> 24);  /* bit[30:24]: log2(linesize) */

  loongarch_cpu_cache[CPU_NATIVE].l1d_size
    = (1 << ((l1_szword & 0x00ff0000) >> 16)) /* bit[23:16]: log2(idx) */
    * ((l1_szword & 0x0000ffff) + 1)	      /* bit[15:0]:  sets - 1 */
    * (1 << ((l1_szword & 0x7f000000) >> 24)) /* bit[30:24]: log2(linesize) */
    >> 10;				      /* in kilobytes */

  loongarch_cpu_cache[CPU_NATIVE].l2d_size
    = (1 << ((l2_szword & 0x00ff0000) >> 16)) /* bit[23:16]: log2(idx) */
    * ((l2_szword & 0x0000ffff) + 1)	      /* bit[15:0]:  sets - 1 */
    * (1 << ((l2_szword & 0x7f000000) >> 24)) /* bit[30:24]: log2(linesize) */
    >> 10;				      /* in kilobytes */

  /* Fill: ret_cpu_type
     With: processor ID (PRID)
     At:   cpucfg_words[0][31:0] */

  switch (cpucfg_cache[0] & 0x00ffff00)
  {
    case 0x0014c000:   /* LA464 */
      ret_cpu_type = CPU_LA464;
      break;

    default:
      /* Unknown PRID.  This is generally harmless as long as
	 the properties above can be obtained via "cpucfg".  */
      if (p_tune_native)
	inform (UNKNOWN_LOCATION, "unknown processor ID %<0x%x%>, "
		"some tuning parameters will fall back to default",
		cpucfg_cache[0]);
      break;
  }

  /* Properties that cannot be looked up directly using cpucfg.  */
  loongarch_cpu_issue_rate[CPU_NATIVE]
    = loongarch_cpu_issue_rate[ret_cpu_type];

  loongarch_cpu_multipass_dfa_lookahead[CPU_NATIVE]
    = loongarch_cpu_multipass_dfa_lookahead[ret_cpu_type];

  loongarch_cpu_rtx_cost_data[CPU_NATIVE]
    = loongarch_cpu_rtx_cost_data[ret_cpu_type];

  return ret_cpu_type;
}
