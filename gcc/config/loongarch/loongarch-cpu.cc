/* Definitions for LoongArch CPU properties.
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
#include "diagnostic-core.h"

#include "loongarch-def.h"
#include "loongarch-opts.h"
#include "loongarch-cpu.h"
#include "loongarch-str.h"
#include "loongarch-evolution.h"


/* Native CPU detection with "cpucfg" */
static uint32_t cpucfg_cache[N_CPUCFG_WORDS] = { 0 };

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
  for (int idx: cpucfg_useful_idx)
    cpucfg_cache[idx] = read_cpucfg_word (idx);
}

uint32_t
get_native_prid (void)
{
  /* Fill loongarch_cpu_default_config[ARCH_NATIVE] with cpucfg data,
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

/* Fill property tables for ARCH_NATIVE / TUNE_NATIVE.  */
void
fill_native_cpu_config (struct loongarch_target *tgt)
{
  int arch_native_p = tgt->cpu_arch == ARCH_NATIVE;
  int tune_native_p = tgt->cpu_tune == TUNE_NATIVE;
  int native_cpu_arch = ARCH_NATIVE;
  int native_cpu_tune = TUNE_NATIVE;

  /* Nothing needs to be done unless "-march/tune=native"
     is given or implied.  */
  if (!arch_native_p && !tune_native_p)
    return;

  /* Fill cpucfg_cache with the "cpucfg" instruction.  */
  cache_cpucfg ();

  /* Fill: tgt->cpu_arch | tgt->cpu_tune
     With: processor ID (PRID)
     At:   cpucfg_words[0][31:0] */

  switch (cpucfg_cache[0] & 0x00ffff00)
  {
    case 0x0014c000:   /* LA464 */
      native_cpu_arch = ARCH_LA464;
      native_cpu_tune = TUNE_LA464;
      break;

    case 0x0014d000:   /* LA664 */
      native_cpu_arch = ARCH_LA664;
      native_cpu_tune = TUNE_LA664;
      break;

    default:
      /* Unknown PRID.  */
      if (tune_native_p)
	inform (UNKNOWN_LOCATION, "unknown processor ID %<0x%x%>, "
		"some tuning parameters will fall back to default",
		cpucfg_cache[0]);
      break;
  }

  /* if -march=native */
  if (arch_native_p)
    {
      int tmp;
      tgt->cpu_arch = native_cpu_arch;

      auto &preset = loongarch_cpu_default_isa[tgt->cpu_arch];

      /* Fill: loongarch_cpu_default_isa[tgt->cpu_arch].base
	 With: base architecture (ARCH)
	 At:   cpucfg_words[1][1:0] */

      if (native_cpu_arch != ARCH_NATIVE)
	tmp = loongarch_cpu_default_isa[native_cpu_arch].base;
      else
	switch (cpucfg_cache[1] & 0x3)
	  {
	    case 0x02:
	      tmp = ISA_BASE_LA64;
	      break;

	    default:
	      fatal_error (UNKNOWN_LOCATION,
			   "unknown native base architecture %<0x%x%>, "
			   "%qs failed",
			   (unsigned int) (cpucfg_cache[1] & 0x3),
			   "-m" OPTSTR_ARCH "=" STR_CPU_NATIVE);
	  }

      /* Use the native value anyways.  */
      preset.base = tmp;

      /* Fill: loongarch_cpu_default_isa[tgt->cpu_arch].fpu
	 With: FPU type (FP, FP_SP, FP_DP)
	 At:   cpucfg_words[2][2:0] */

      switch (cpucfg_cache[2] & 0x7)
	{
	  case 0x07:
	    tmp = ISA_EXT_FPU64;
	    break;

	  case 0x03:
	    tmp = ISA_EXT_FPU32;
	    break;

	  case 0x00:
	    tmp = ISA_EXT_NONE;
	    break;

	  default:
	    fatal_error (UNKNOWN_LOCATION,
			 "unknown native FPU type %<0x%x%>, %qs failed",
			 (unsigned int) (cpucfg_cache[2] & 0x7),
			 "-m" OPTSTR_ARCH "=" STR_CPU_NATIVE);
	}

      /* Check consistency with PRID presets.  */
      if (native_cpu_arch != ARCH_NATIVE && tmp != preset.fpu)
	warning (0, "floating-point unit %qs differs from PRID preset %qs",
		 loongarch_isa_ext_strings[tmp],
		 loongarch_isa_ext_strings[preset.fpu]);

      /* Use the native value anyways.  */
      preset.fpu = tmp;


      /* Fill: loongarch_cpu_default_isa[ARCH_NATIVE].simd
	 With: SIMD extension type (LSX, LASX)
	 At:   cpucfg_words[2][7:6] */

      switch (cpucfg_cache[2] & 0xc0)
	{
	  case 0xc0:
	    tmp = ISA_EXT_SIMD_LASX;
	    break;

	  case 0x40:
	    tmp = ISA_EXT_SIMD_LSX;
	    break;

	  case 0x80:
	    tmp = 0;
	    warning (0, "unknown SIMD extension "
			"(%qs disabled while %qs is enabled), disabling SIMD",
			loongarch_isa_ext_strings[ISA_EXT_SIMD_LSX],
			loongarch_isa_ext_strings[ISA_EXT_SIMD_LASX]);
	    break;

	  case 0x00:
	    tmp = 0;
	    break;
	}

      /* Check consistency with PRID presets.  */

      /*
      if (native_cpu_arch != ARCH_NATIVE && tmp != preset.simd)
	warning (0, "SIMD extension %qs differs from PRID preset %qs",
		 loongarch_isa_ext_strings[tmp],
		 loongarch_isa_ext_strings[preset.simd]);
      */

      /* Use the native value anyways.  */
      preset.simd = tmp;


      int64_t hw_isa_evolution = 0;

      /* Features added during ISA evolution.  */
      for (const auto &entry: cpucfg_map)
	if (cpucfg_cache[entry.cpucfg_word] & entry.cpucfg_bit)
	  hw_isa_evolution |= entry.isa_evolution_bit;

      if (native_cpu_arch != ARCH_NATIVE)
	{
	  /* Check if the local CPU really supports the features of the base
	     ISA of probed native_cpu_arch.  If any feature is not detected,
	     either GCC or the hardware is buggy.  */
	  if ((preset.evolution & hw_isa_evolution) != hw_isa_evolution)
	    warning (0,
		     "detected base architecture %qs, but some of its "
		     "features are not detected; the detected base "
		     "architecture may be unreliable, only detected "
		     "features will be enabled",
		     loongarch_isa_base_strings[preset.base]);
	}
      preset.evolution = hw_isa_evolution;
    }

  if (tune_native_p)
    {
      tgt->cpu_tune = native_cpu_tune;

      /* Fill: loongarch_cpu_cache[tgt->cpu_tune]
	 With: cache size info
	 At:   cpucfg_words[16:20][31:0] */

      auto &preset_cache = loongarch_cpu_cache[tgt->cpu_tune];
      struct loongarch_cache native_cache;
      int l1d_present = 0, l1u_present = 0;
      int l2d_present = 0;
      uint32_t l1_szword, l2_szword;

      l1u_present |= cpucfg_cache[16] & 3;	  /* bit[1:0]: unified l1 */
      l1d_present |= cpucfg_cache[16] & 4;	  /* bit[2:2]: l1d */
      l1_szword = l1d_present ? 18 : (l1u_present ? 17 : 0);
      l1_szword = l1_szword ? cpucfg_cache[l1_szword] : 0;

      l2d_present |= cpucfg_cache[16] & 24;	  /* bit[4:3]: unified l2 */
      l2d_present |= cpucfg_cache[16] & 128;	  /* bit[7:7]: l2d */
      l2_szword = l2d_present ? cpucfg_cache[19] : 0;

      native_cache.l1d_line_size
	= 1 << ((l1_szword & 0x7f000000) >> 24);  /* bit[30:24]: log2(line) */

      native_cache.l1d_size
	= (1 << ((l1_szword & 0x00ff0000) >> 16)) /* bit[23:16]: log2(idx) */
	* ((l1_szword & 0x0000ffff) + 1)	  /* bit[15:0]:  sets - 1 */
	* (1 << ((l1_szword & 0x7f000000) >> 24)) /* bit[30:24]: log2(line) */
	>> 10;					  /* in kibibytes */

      native_cache.l2d_size
	= (1 << ((l2_szword & 0x00ff0000) >> 16)) /* bit[23:16]: log2(idx) */
	* ((l2_szword & 0x0000ffff) + 1)	  /* bit[15:0]:  sets - 1 */
	* (1 << ((l2_szword & 0x7f000000) >> 24)) /* bit[30:24]: log2(linesz) */
	>> 10;					  /* in kibibytes */

      /* Use the native value anyways.  */
      preset_cache.l1d_line_size = native_cache.l1d_line_size;
      preset_cache.l1d_size = native_cache.l1d_size;
      preset_cache.l2d_size = native_cache.l2d_size;
    }
}
