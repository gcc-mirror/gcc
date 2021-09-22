/* Definitions for LoongArch CPU properties.
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

#include "loongarch-cpu.h"

/* CPU property tables */
const char* loongarch_cpu_strings[] = {
  /* CPU_NATIVE */      "native",
  /* CPU_LOONGARCH64 */ "loongarch64",
  /* CPU_GS464V */      "gs464v",
};

#define N M_OPTION_NOT_SEEN
struct loongarch_cpu_config loongarch_cpu_default_config[] = {
  /* CPU_NATIVE */      { N, N, N, N,
			  { N } },
  /* CPU_LOONGARCH64 */ { ISA_LA64, ABI_LP64, ISA_DOUBLE_FLOAT, ABI_DOUBLE_FLOAT,
			  { 1 } },
  /* CPU_GS464V */      { ISA_LA64, ABI_LP64, ISA_DOUBLE_FLOAT, ABI_DOUBLE_FLOAT,
			  { 1 } },
};
#undef N

int loongarch_cpu_issue_rate[] = {
  /* CPU_NATIVE */      0,
  /* CPU_LOONGARCH64 */ 4,
  /* CPU_GS464V */      4,
};

int loongarch_cpu_multipass_dfa_lookahead[] = {
  /* CPU_NATIVE */      0,
  /* CPU_LOONGARCH64 */ 4,
  /* CPU_GS464V */      4,
};

/* RTX costs to use when optimizing for speed, indexed by processor.  */
struct loongarch_rtx_cost_data
loongarch_cpu_rtx_cost_data[] = {
  /* CPU_NATIVE */ {
      DEFAULT_COSTS
  },
  /* CPU_LOONGARCH64 */ {
      DEFAULT_COSTS
  },
  /* CPU_GS464V */  {
      DEFAULT_COSTS
  },
};

/* Native CPU detection logic with "cpucfg" */
#define LARCH_CPUCFG_WORDS 15
static uint32_t cpucfg_cache[LARCH_CPUCFG_WORDS];

static int
read_cpucfg_word (int wordno)
{
  /* To make cross-compiler shut up.  */
  int ret = wordno & 0;

  #ifdef __loongarch__
  __asm__ __volatile__ ("cpucfg %0,%1\n\t"
			:"=r"(ret)
			:"r"(wordno)
			:);
  #endif

  return ret;
}

void cache_cpucfg ()
{
  for (int i = 0; i < LARCH_CPUCFG_WORDS; i++)
    cpucfg_cache[i] = read_cpucfg_word(i);
}

/* Fill loongarch_cpu_default_config[CPU_NATIVE] with cpucfg data,
   see "Loongson Architecture Reference Manual"
   (Volume 1, Section 2.2.10.5) */

uint32_t get_native_prid()
{
  return cpucfg_cache[0];
}

int fill_native_cpu_config()
{
  int int_isa = -1, int_abi = -1;
  int float_isa = -1, float_abi = -1;
  int cpu_type = -1;

  /* Fill: loongarch_cpu_default_config[CPU_NATIVE].isa_int
     With: Integer ISA (ARCH)
     At:   word[1][1:0] */

  switch (cpucfg_cache[1] & 0x03)
    {
      case 0x02:
	int_isa = ISA_LA64;
	int_abi = ABI_LP64;
	break;
    }

  /* Fill: loongarch_cpu_default_config[CPU_NATIVE].isa_float
     With: Floating-point ISA (FP, FP_SP, FP_DP)
     At:   word[1][1:0] */

  switch (cpucfg_cache[2] & 0x07)
    {
      case 0x07:
	float_isa = ISA_DOUBLE_FLOAT;
	float_abi = ABI_DOUBLE_FLOAT;
	break;

      case 0x03:
	float_isa = ISA_SINGLE_FLOAT;
	float_abi = ABI_SINGLE_FLOAT;
	break;

      case 0x00:
	float_isa = ISA_SOFT_FLOAT;
	float_abi = ABI_SOFT_FLOAT;
	break;
    }


  /* Fill: loongarch_cpu_default_config[CPU_NATIVE]
     With: Processor ID (PRID)
     At:   cpucfg_words[0][31:0] */

  switch (cpucfg_cache[0] & 0x00ffff00)
  {
    case 0x0014c000:   /* GS464V */
      cpu_type = CPU_GS464V;
      break;

    default:
      /* FIXME: GCC shouldn't complain about this,
	but current we still rely on static processor feature tables
	to provide safe fall-back settings.*/
      return -1;
  }

  /* Fill the default tables */
  loongarch_cpu_default_config [CPU_NATIVE]
    = loongarch_cpu_default_config [cpu_type];

  #define NATIVE_INT_ISA (loongarch_cpu_default_config[CPU_NATIVE].isa_int)
  #define NATIVE_INT_ABI (loongarch_cpu_default_config[CPU_NATIVE].abi_int)
  #define NATIVE_FLOAT_ISA (loongarch_cpu_default_config[CPU_NATIVE].isa_float)
  #define NATIVE_FLOAT_ABI (loongarch_cpu_default_config[CPU_NATIVE].abi_float)

  if (int_isa != -1 && NATIVE_INT_ISA != int_isa)
    {
      NATIVE_INT_ISA = int_isa;
      NATIVE_INT_ABI = int_abi;
    }

  if (float_isa != -1 && NATIVE_FLOAT_ISA != float_isa)
    {
      NATIVE_FLOAT_ISA = float_isa;
      NATIVE_FLOAT_ABI = float_abi;
    }

  loongarch_cpu_issue_rate [CPU_NATIVE]
    = loongarch_cpu_issue_rate [cpu_type];

  loongarch_cpu_multipass_dfa_lookahead [CPU_NATIVE]
    = loongarch_cpu_multipass_dfa_lookahead [cpu_type];

  loongarch_cpu_rtx_cost_data [CPU_NATIVE]
    = loongarch_cpu_rtx_cost_data [cpu_type];

  return cpu_type;
}
