/* Subroutines for the gcc driver.
   Copyright (C) 2006-2025 Free Software Foundation, Inc.

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
#include "diagnostic.h"

const char *host_detect_local_cpu (int argc, const char **argv);

#if defined(__GNUC__) && (__GNUC__ >= 5 || !defined(__PIC__))
#include "cpuid.h"
#include "common/config/i386/cpuinfo.h"
#include "common/config/i386/i386-isas.h"

struct cache_desc
{
  unsigned sizekb;
  unsigned assoc;
  unsigned line;
};

/* Returns command line parameters that describe size and
   cache line size of the processor caches.  */

static char *
describe_cache (struct cache_desc level1, struct cache_desc level2)
{
  char size[100], line[100], size2[100];

  /* At the moment, gcc does not use the information
     about the associativity of the cache.  */

  snprintf (size, sizeof (size),
	    "--param l1-cache-size=%u ", level1.sizekb);
  snprintf (line, sizeof (line),
	    "--param l1-cache-line-size=%u ", level1.line);

  snprintf (size2, sizeof (size2),
	    "--param l2-cache-size=%u ", level2.sizekb);

  return concat (size, line, size2, NULL);
}

/* Detect L2 cache parameters using CPUID extended function 0x80000006.  */

static void
detect_l2_cache (struct cache_desc *level2)
{
  unsigned eax, ebx, ecx, edx;
  unsigned assoc;

  __cpuid (0x80000006, eax, ebx, ecx, edx);

  level2->sizekb = (ecx >> 16) & 0xffff;
  level2->line = ecx & 0xff;

  assoc = (ecx >> 12) & 0xf;
  if (assoc == 6)
    assoc = 8;
  else if (assoc == 8)
    assoc = 16;
  else if (assoc >= 0xa && assoc <= 0xc)
    assoc = 32 + (assoc - 0xa) * 16;
  else if (assoc >= 0xd && assoc <= 0xe)
    assoc = 96 + (assoc - 0xd) * 32;

  level2->assoc = assoc;
}

/* Returns the description of caches for an AMD processor.  */

static const char *
detect_caches_amd (unsigned max_ext_level)
{
  unsigned eax, ebx, ecx, edx;

  struct cache_desc level1, level2 = {0, 0, 0};

  if (max_ext_level < 0x80000005)
    return "";

  __cpuid (0x80000005, eax, ebx, ecx, edx);

  level1.sizekb = (ecx >> 24) & 0xff;
  level1.assoc = (ecx >> 16) & 0xff;
  level1.line = ecx & 0xff;

  if (max_ext_level >= 0x80000006)
    detect_l2_cache (&level2);

  return describe_cache (level1, level2);
}

/* Decodes the size, the associativity and the cache line size of
   L1/L2 caches of an Intel processor.  Values are based on
   "Intel Processor Identification and the CPUID Instruction"
   [Application Note 485], revision -032, December 2007.  */

static void
decode_caches_intel (unsigned reg, bool xeon_mp,
		     struct cache_desc *level1, struct cache_desc *level2)
{
  int i;

  for (i = 24; i >= 0; i -= 8)
    switch ((reg >> i) & 0xff)
      {
      case 0x0a:
	level1->sizekb = 8; level1->assoc = 2; level1->line = 32;
	break;
      case 0x0c:
	level1->sizekb = 16; level1->assoc = 4; level1->line = 32;
	break;
      case 0x0d:
	level1->sizekb = 16; level1->assoc = 4; level1->line = 64;
	break;
      case 0x0e:
	level1->sizekb = 24; level1->assoc = 6; level1->line = 64;
	break;
      case 0x21:
	level2->sizekb = 256; level2->assoc = 8; level2->line = 64;
	break;
      case 0x24:
	level2->sizekb = 1024; level2->assoc = 16; level2->line = 64;
	break;
      case 0x2c:
	level1->sizekb = 32; level1->assoc = 8; level1->line = 64;
	break;
      case 0x39:
	level2->sizekb = 128; level2->assoc = 4; level2->line = 64;
	break;
      case 0x3a:
	level2->sizekb = 192; level2->assoc = 6; level2->line = 64;
	break;
      case 0x3b:
	level2->sizekb = 128; level2->assoc = 2; level2->line = 64;
	break;
      case 0x3c:
	level2->sizekb = 256; level2->assoc = 4; level2->line = 64;
	break;
      case 0x3d:
	level2->sizekb = 384; level2->assoc = 6; level2->line = 64;
	break;
      case 0x3e:
	level2->sizekb = 512; level2->assoc = 4; level2->line = 64;
	break;
      case 0x41:
	level2->sizekb = 128; level2->assoc = 4; level2->line = 32;
	break;
      case 0x42:
	level2->sizekb = 256; level2->assoc = 4; level2->line = 32;
	break;
      case 0x43:
	level2->sizekb = 512; level2->assoc = 4; level2->line = 32;
	break;
      case 0x44:
	level2->sizekb = 1024; level2->assoc = 4; level2->line = 32;
	break;
      case 0x45:
	level2->sizekb = 2048; level2->assoc = 4; level2->line = 32;
	break;
      case 0x48:
	level2->sizekb = 3072; level2->assoc = 12; level2->line = 64;
	break;
      case 0x49:
	if (xeon_mp)
	  break;
	level2->sizekb = 4096; level2->assoc = 16; level2->line = 64;
	break;
      case 0x4e:
	level2->sizekb = 6144; level2->assoc = 24; level2->line = 64;
	break;
      case 0x60:
	level1->sizekb = 16; level1->assoc = 8; level1->line = 64;
	break;
      case 0x66:
	level1->sizekb = 8; level1->assoc = 4; level1->line = 64;
	break;
      case 0x67:
	level1->sizekb = 16; level1->assoc = 4; level1->line = 64;
	break;
      case 0x68:
	level1->sizekb = 32; level1->assoc = 4; level1->line = 64;
	break;
      case 0x78:
	level2->sizekb = 1024; level2->assoc = 4; level2->line = 64;
	break;
      case 0x79:
	level2->sizekb = 128; level2->assoc = 8; level2->line = 64;
	break;
      case 0x7a:
	level2->sizekb = 256; level2->assoc = 8; level2->line = 64;
	break;
      case 0x7b:
	level2->sizekb = 512; level2->assoc = 8; level2->line = 64;
	break;
      case 0x7c:
	level2->sizekb = 1024; level2->assoc = 8; level2->line = 64;
	break;
      case 0x7d:
	level2->sizekb = 2048; level2->assoc = 8; level2->line = 64;
	break;
      case 0x7f:
	level2->sizekb = 512; level2->assoc = 2; level2->line = 64;
	break;
      case 0x80:
	level2->sizekb = 512; level2->assoc = 8; level2->line = 64;
	break;
      case 0x82:
	level2->sizekb = 256; level2->assoc = 8; level2->line = 32;
	break;
      case 0x83:
	level2->sizekb = 512; level2->assoc = 8; level2->line = 32;
	break;
      case 0x84:
	level2->sizekb = 1024; level2->assoc = 8; level2->line = 32;
	break;
      case 0x85:
	level2->sizekb = 2048; level2->assoc = 8; level2->line = 32;
	break;
      case 0x86:
	level2->sizekb = 512; level2->assoc = 4; level2->line = 64;
	break;
      case 0x87:
	level2->sizekb = 1024; level2->assoc = 8; level2->line = 64;

      default:
	break;
      }
}

/* Detect cache parameters using CPUID function 2.  */

static void
detect_caches_cpuid2 (bool xeon_mp,
		      struct cache_desc *level1, struct cache_desc *level2)
{
  unsigned regs[4];
  int nreps, i;

  __cpuid (2, regs[0], regs[1], regs[2], regs[3]);

  nreps = regs[0] & 0x0f;
  regs[0] &= ~0x0f;

  while (--nreps >= 0)
    {
      for (i = 0; i < 4; i++)
	if (regs[i] && !((regs[i] >> 31) & 1))
	  decode_caches_intel (regs[i], xeon_mp, level1, level2);

      if (nreps)
	__cpuid (2, regs[0], regs[1], regs[2], regs[3]);
    }
}

/* Detect cache parameters using CPUID function 4. This
   method doesn't require hardcoded tables.  */

enum cache_type
{
  CACHE_END = 0,
  CACHE_DATA = 1,
  CACHE_INST = 2,
  CACHE_UNIFIED = 3
};

static void
detect_caches_cpuid4 (struct cache_desc *level1, struct cache_desc *level2,
		      struct cache_desc *level3)
{
  struct cache_desc *cache;

  unsigned eax, ebx, ecx, edx;
  int count;

  for (count = 0;; count++)
    {
      __cpuid_count(4, count, eax, ebx, ecx, edx);
      switch (eax & 0x1f)
	{
	case CACHE_END:
	  return;
	case CACHE_DATA:
	case CACHE_UNIFIED:
	  {
	    switch ((eax >> 5) & 0x07)
	      {
	      case 1:
		cache = level1;
		break;
	      case 2:
		cache = level2;
		break;
	      case 3:
		cache = level3;
		break;
	      default:
		cache = NULL;
	      }

	    if (cache)
	      {
		unsigned sets = ecx + 1;
		unsigned part = ((ebx >> 12) & 0x03ff) + 1;

		cache->assoc = ((ebx >> 22) & 0x03ff) + 1;
		cache->line = (ebx & 0x0fff) + 1;

		cache->sizekb = (cache->assoc * part
				 * cache->line * sets) / 1024;
	      }
	  }
	default:
	  break;
	}
    }
}

/* Returns the description of caches for an Intel processor.  */

static const char *
detect_caches_intel (bool xeon_mp, unsigned max_level,
		     unsigned max_ext_level, unsigned *l2sizekb)
{
  struct cache_desc level1 = {0, 0, 0}, level2 = {0, 0, 0}, level3 = {0, 0, 0};

  if (max_level >= 4)
    detect_caches_cpuid4 (&level1, &level2, &level3);
  else if (max_level >= 2)
    detect_caches_cpuid2 (xeon_mp, &level1, &level2);
  else
    return "";

  if (level1.sizekb == 0)
    return "";

  /* Let the L3 replace the L2. This assumes inclusive caches
     and single threaded program for now. */
  if (level3.sizekb)
    level2 = level3;

  /* Intel CPUs are equipped with AMD style L2 cache info.  Try this
     method if other methods fail to provide L2 cache parameters.  */
  if (level2.sizekb == 0 && max_ext_level >= 0x80000006)
    detect_l2_cache (&level2);

  *l2sizekb = level2.sizekb;

  return describe_cache (level1, level2);
}

/* Extended features */
#define has_feature(f) \
  has_cpu_feature (&cpu_model, cpu_features2, f)

/* We will emit a warning when using AVX10.1 and AVX512 options with one
   enabled and the other disabled.  Add this function to avoid push "-mno-"
   options under this scenario for -march=native.  */

bool check_avx512_features (__processor_model &cpu_model,
			    unsigned int (&cpu_features2)[SIZE_OF_CPU_FEATURES],
			    const enum processor_features feature)
{
  if (has_feature (FEATURE_AVX10_1_256)
      && ((feature == FEATURE_AVX512F)
	  || (feature == FEATURE_AVX512CD)
	  || (feature == FEATURE_AVX512DQ)
	  || (feature == FEATURE_AVX512BW)
	  || (feature == FEATURE_AVX512VL)
	  || (feature == FEATURE_AVX512IFMA)
	  || (feature == FEATURE_AVX512VBMI)
	  || (feature == FEATURE_AVX512VBMI2)
	  || (feature == FEATURE_AVX512VNNI)
	  || (feature == FEATURE_AVX512VPOPCNTDQ)
	  || (feature == FEATURE_AVX512BITALG)
	  || (feature == FEATURE_AVX512FP16)
	  || (feature == FEATURE_AVX512BF16)))
    return false;

  return true;
}

/* This will be called by the spec parser in gcc.cc when it sees
   a %:local_cpu_detect(args) construct.  Currently it will be
   called with either "arch [32|64]" or "tune [32|64]" as argument
   depending on if -march=native or -mtune=native is to be substituted.

   It returns a string containing new command line parameters to be
   put at the place of the above two options, depending on what CPU
   this is executed.  E.g. "-march=k8" on an AMD64 machine
   for -march=native.

   ARGC and ARGV are set depending on the actual arguments given
   in the spec.  */

const char *host_detect_local_cpu (int argc, const char **argv)
{
  enum processor_type processor = PROCESSOR_I386;
  const char *cpu = "i386";

  const char *cache = "";
  const char *options = "";

  unsigned int ebx, ecx, edx;

  unsigned int max_level, ext_level;

  unsigned int vendor;
  unsigned int model, family;

  bool arch;

  unsigned int l2sizekb = 0;

  if (argc < 2)
    return NULL;

  arch = !strcmp (argv[0], "arch");

  if (!arch && strcmp (argv[0], "tune"))
    return NULL;

  bool codegen_x86_64;

  if (!strcmp (argv[1], "32"))
    codegen_x86_64 = false;
  else if (!strcmp (argv[1], "64"))
    codegen_x86_64 = true;
  else
    return NULL;

  struct __processor_model cpu_model = { };
  struct __processor_model2 cpu_model2 = { };
  unsigned int cpu_features2[SIZE_OF_CPU_FEATURES] = { };

  if (cpu_indicator_init (&cpu_model, &cpu_model2, cpu_features2) != 0)
    goto done;

  vendor = cpu_model.__cpu_vendor;
  family = cpu_model2.__cpu_family;
  model = cpu_model2.__cpu_model;
  max_level = cpu_model2.__cpu_max_level;
  ext_level = cpu_model2.__cpu_ext_level;

  if (!arch)
    {
      if (vendor == VENDOR_AMD
	  || vendor == VENDOR_CENTAUR
	  || vendor == VENDOR_CYRIX
	  || vendor == VENDOR_NSC)
	cache = detect_caches_amd (ext_level);
      else if (vendor == VENDOR_INTEL
			 || vendor == VENDOR_ZHAOXIN)
	{
	  bool xeon_mp = (family == 15 && model == 6);
	  cache = detect_caches_intel (xeon_mp, max_level,
				       ext_level, &l2sizekb);
	}
    }

  if (vendor == VENDOR_AMD)
    {
      unsigned int name;

      /* Detect geode processor by its processor signature.  */
      if (ext_level >= 0x80000002)
	__cpuid (0x80000002, name, ebx, ecx, edx);
      else
	name = 0;

      if (name == signature_NSC_ebx)
	processor = PROCESSOR_GEODE;
      else if (has_feature (FEATURE_MOVBE) && family == 22)
	processor = PROCESSOR_BTVER2;
      else if (has_feature (FEATURE_AVX512VP2INTERSECT))
	processor = PROCESSOR_ZNVER5;
      else if (has_feature (FEATURE_AVX512F))
	processor = PROCESSOR_ZNVER4;
      else if (has_feature (FEATURE_VAES))
	processor = PROCESSOR_ZNVER3;
      else if (has_feature (FEATURE_CLWB))
	processor = PROCESSOR_ZNVER2;
      else if (has_feature (FEATURE_CLZERO))
	processor = PROCESSOR_ZNVER1;
      else if (has_feature (FEATURE_AVX2))
	processor = PROCESSOR_BDVER4;
      else if (has_feature (FEATURE_XSAVEOPT))
	processor = PROCESSOR_BDVER3;
      else if (has_feature (FEATURE_BMI))
	processor = PROCESSOR_BDVER2;
      else if (has_feature (FEATURE_XOP))
	processor = PROCESSOR_BDVER1;
      else if (has_feature (FEATURE_SSE4_A)
	       && has_feature (FEATURE_SSSE3))
	processor = PROCESSOR_BTVER1;
      else if (has_feature (FEATURE_SSE4_A))
	processor = PROCESSOR_AMDFAM10;
      else if (has_feature (FEATURE_SSE2)
	       || has_feature (FEATURE_LM))
	processor = PROCESSOR_K8;
      else if (has_feature (FEATURE_3DNOWP) && family == 6)
	processor = PROCESSOR_ATHLON;
      else if (has_feature (FEATURE_MMX))
	processor = PROCESSOR_K6;
      else
	processor = PROCESSOR_PENTIUM;
    }
  else if (vendor == VENDOR_CENTAUR)
    {
      processor = PROCESSOR_GENERIC;

      switch (family)
	{
	default:
	  /* We have no idea.  */
	  break;

	case 5:
	  if (has_feature (FEATURE_3DNOW)
	      || has_feature (FEATURE_MMX))
	    processor = PROCESSOR_I486;
	  break;

	case 6:
	  if (has_feature (FEATURE_LM))
	    processor = PROCESSOR_K8;
	  else if (model >= 9)
	    processor = PROCESSOR_PENTIUMPRO;
	  else if (model >= 6)
	    processor = PROCESSOR_I486;
	}
    }
  else if (vendor == VENDOR_ZHAOXIN)
    {
      processor = PROCESSOR_GENERIC;

      switch (family)
	{
	case 7:
	  if (model >= 0x6b)
	    processor = PROCESSOR_SHIJIDADAO;
	  else if (model == 0x5b)
	    processor = PROCESSOR_YONGFENG;
	  else if (model == 0x3b)
	    processor = PROCESSOR_LUJIAZUI;
	  break;
	default:
	  break;
	}
    }
  else
    {
      switch (family)
	{
	case 4:
	  processor = PROCESSOR_I486;
	  break;
	case 5:
	  processor = PROCESSOR_PENTIUM;
	  break;
	case 6:
	case 19:
	  processor = PROCESSOR_PENTIUMPRO;
	  break;
	case 15:
	  processor = PROCESSOR_PENTIUM4;
	  break;
	default:
	  /* We have no idea.  */
	  processor = PROCESSOR_GENERIC;
	}
    }

  switch (processor)
    {
    case PROCESSOR_I386:
      /* Default.  */
      break;
    case PROCESSOR_I486:
      if (arch && vendor == VENDOR_CENTAUR)
	{
	  if (model >= 6)
	    cpu = "c3";
	  else if (has_feature (FEATURE_3DNOW))
	    cpu = "winchip2";
	  else
	    /* Assume WinChip C6.  */
	    cpu = "winchip-c6";
	}
      else
	cpu = "i486";
      break;
    case PROCESSOR_PENTIUM:
      if (arch && has_feature (FEATURE_MMX))
	cpu = "pentium-mmx";
      else
	cpu = "pentium";
      break;
    case PROCESSOR_PENTIUMPRO:
      cpu = get_intel_cpu (&cpu_model, &cpu_model2, cpu_features2);
      if (cpu == NULL)
	{
	  if (arch)
	    {
	      /* This is unknown CPU.  */
	      if (has_feature (FEATURE_AVX512F))
		{
		  /* Assume Diamond Rapids.  */
		  if (has_feature (FEATURE_AVX10_2_512))
		    cpu = "diamondrapids";
		  /* Assume Granite Rapids D.  */
		  else if (has_feature (FEATURE_AMX_COMPLEX))
		    cpu = "graniterapids-d";
		  /* Assume Granite Rapids.  */
		  else if (has_feature (FEATURE_AMX_FP16))
		    cpu = "graniterapids";
		  /* Assume Tiger Lake */
		  else if (has_feature (FEATURE_AVX512VP2INTERSECT))
		    cpu = "tigerlake";
		  /* Assume Sapphire Rapids.  */
		  else if (has_feature (FEATURE_TSXLDTRK))
		    cpu = "sapphirerapids";
		  /* Assume Cooper Lake */
		  else if (has_feature (FEATURE_AVX512BF16))
		    cpu = "cooperlake";
		  /* Assume Ice Lake Server.  */
		  else if (has_feature (FEATURE_WBNOINVD))
		    cpu = "icelake-server";
		  /* Assume Ice Lake.  */
		  else if (has_feature (FEATURE_AVX512BITALG))
		    cpu = "icelake-client";
		  /* Assume Cannon Lake.  */
		  else if (has_feature (FEATURE_AVX512VBMI))
		    cpu = "cannonlake";
		  /* Assume Xeon Phi Processors.  Support has been removed
		     since GCC 15.  */
		  else if (!has_feature (FEATURE_AVX512VL))
		    error ("Xeon Phi ISA support has been removed since "
			   "GCC 15, use GCC 14 for the Xeon Phi ISAs or "
			   "%<-march=broadwell%> for all the other ISAs "
			   "supported on this machine.");
		  /* Assume Skylake with AVX-512.  */
		  else
		    cpu = "skylake-avx512";
		}
	      else if (has_feature (FEATURE_AVX))
		{
		  /* Assume Panther Lake.  */
		  if (has_feature (FEATURE_PREFETCHI))
		    cpu = "pantherlake";
		  /* Assume Clearwater Forest.  */
		  else if (has_feature (FEATURE_USER_MSR))
		    cpu = "clearwaterforest";
		  /* Assume Arrow Lake S.  */
		  else if (has_feature (FEATURE_SM3))
		    cpu = "arrowlake-s";
		  /* Assume Sierra Forest.  */
		  else if (has_feature (FEATURE_AVXVNNIINT8))
		    cpu = "sierraforest";
		  /* Assume Alder Lake.  */
		  else if (has_feature (FEATURE_SERIALIZE))
		    cpu = "alderlake";
		  /* Assume Skylake.  */
		  else if (has_feature (FEATURE_CLFLUSHOPT))
		    cpu = "skylake";
		  /* Assume Broadwell.  */
		  else if (has_feature (FEATURE_ADX))
		    cpu = "broadwell";
		  /* Assume Haswell.  */
		  else if (has_feature (FEATURE_AVX2))
		    cpu = "haswell";
		  /* Assume Sandy Bridge.  */
		  else
		    cpu = "sandybridge";
	      }
	      else if (has_feature (FEATURE_SSE4_2))
		{
		  if (has_feature (FEATURE_GFNI))
		    /* Assume Tremont.  */
		    cpu = "tremont";
		  else if (has_feature (FEATURE_SGX))
		    /* Assume Goldmont Plus.  */
		    cpu = "goldmont-plus";
		  else if (has_feature (FEATURE_XSAVE))
		    /* Assume Goldmont.  */
		    cpu = "goldmont";
		  else if (has_feature (FEATURE_MOVBE))
		    /* Assume Silvermont.  */
		    cpu = "silvermont";
		  else
		    /* Assume Nehalem.  */
		    cpu = "nehalem";
		}
	      else if (has_feature (FEATURE_SSSE3))
		{
		  if (has_feature (FEATURE_MOVBE))
		    /* Assume Bonnell.  */
		    cpu = "bonnell";
		  else
		    /* Assume Core 2.  */
		    cpu = "core2";
		}
	      else if (has_feature (FEATURE_LM))
		/* Perhaps some emulator?  Assume x86-64, otherwise gcc
		   -march=native would be unusable for 64-bit compilations,
		   as all the CPUs below are 32-bit only.  */
		cpu = "x86-64";
	      else if (has_feature (FEATURE_SSE3))
		{
		  if (vendor == VENDOR_CENTAUR)
		    /* C7 / Eden "Esther" */
		    cpu = "c7";
		  else
		    /* It is Core Duo.  */
		    cpu = "pentium-m";
		}
	      else if (has_feature (FEATURE_SSE2))
		/* It is Pentium M.  */
		cpu = "pentium-m";
	      else if (has_feature (FEATURE_SSE))
		{
		  if (vendor == VENDOR_CENTAUR)
		    {
		      if (model >= 9)
			/* Eden "Nehemiah" */
			cpu = "nehemiah";
		      else
			cpu = "c3-2";
		    }
		  else
		    /* It is Pentium III.  */
		    cpu = "pentium3";
		}
	      else if (has_feature (FEATURE_MMX))
		/* It is Pentium II.  */
		cpu = "pentium2";
	      else
		/* Default to Pentium Pro.  */
		cpu = "pentiumpro";
	    }
	  else
	    /* For -mtune, we default to -mtune=generic.  */
	    cpu = "generic";
	}
      break;
    case PROCESSOR_PENTIUM4:
      if (has_feature (FEATURE_SSE3))
	{
	  if (has_feature (FEATURE_LM))
	    cpu = "nocona";
	  else
	    cpu = "prescott";
	}
      else
	cpu = "pentium4";
      break;
    case PROCESSOR_GEODE:
      cpu = "geode";
      break;
    case PROCESSOR_K6:
      if (arch && has_feature (FEATURE_3DNOW))
	cpu = "k6-3";
      else
	cpu = "k6";
      break;
    case PROCESSOR_ATHLON:
      if (arch && has_feature (FEATURE_SSE))
	cpu = "athlon-4";
      else
	cpu = "athlon";
      break;
    case PROCESSOR_K8:
      if (arch)
	{
	  if (vendor == VENDOR_CENTAUR)
	    {
	      if (has_feature (FEATURE_SSE4_1))
		/* Nano 3000 | Nano dual / quad core | Eden X4 */
		cpu = "nano-3000";
	      else if (has_feature (FEATURE_SSSE3))
		/* Nano 1000 | Nano 2000 */
		cpu = "nano";
	      else if (has_feature (FEATURE_SSE3))
		/* Eden X2 */
		cpu = "eden-x2";
	      else
		/* Default to k8 */
		cpu = "k8";
	    }
	  else if (has_feature (FEATURE_SSE3))
	    cpu = "k8-sse3";
	  else
	    cpu = "k8";
	}
      else
	/* For -mtune, we default to -mtune=k8 */
	cpu = "k8";
      break;
    case PROCESSOR_AMDFAM10:
      cpu = "amdfam10";
      break;
    case PROCESSOR_BDVER1:
      cpu = "bdver1";
      break;
    case PROCESSOR_BDVER2:
      cpu = "bdver2";
      break;
    case PROCESSOR_BDVER3:
      cpu = "bdver3";
      break;
    case PROCESSOR_BDVER4:
      cpu = "bdver4";
      break;
    case PROCESSOR_ZNVER1:
      cpu = "znver1";
      break;
    case PROCESSOR_ZNVER2:
      cpu = "znver2";
      break;
    case PROCESSOR_ZNVER3:
      cpu = "znver3";
      break;
    case PROCESSOR_ZNVER4:
      cpu = "znver4";
      break;
    case PROCESSOR_ZNVER5:
      cpu = "znver5";
      break;
    case PROCESSOR_BTVER1:
      cpu = "btver1";
      break;
    case PROCESSOR_BTVER2:
      cpu = "btver2";
      break;
    case PROCESSOR_LUJIAZUI:
      cpu = "lujiazui";
      break;
    case PROCESSOR_YONGFENG:
      cpu = "yongfeng";
      break;
    case PROCESSOR_SHIJIDADAO:
      cpu = "shijidadao";
      break;

    default:
      /* Use something reasonable.  */
      if (arch)
	{
	  if (has_feature (FEATURE_SSSE3))
	    cpu = "core2";
	  else if (has_feature (FEATURE_SSE3))
	    {
	      if (has_feature (FEATURE_LM))
		cpu = "nocona";
	      else
		cpu = "prescott";
	    }
	  else if (has_feature (FEATURE_LM))
	    /* Perhaps some emulator?  Assume x86-64, otherwise gcc
	       -march=native would be unusable for 64-bit compilations,
	       as all the CPUs below are 32-bit only.  */
	    cpu = "x86-64";
	  else if (has_feature (FEATURE_SSE2))
	    cpu = "pentium4";
	  else if (has_feature (FEATURE_CMOV))
	    cpu = "pentiumpro";
	  else if (has_feature (FEATURE_MMX))
	    cpu = "pentium-mmx";
	  else if (has_feature (FEATURE_CMPXCHG8B))
	    cpu = "pentium";
	}
      else
	cpu = "generic";
    }

  if (arch)
    {
      unsigned int i;
      const char *const neg_option = " -mno-";
      for (i = 0; i < ARRAY_SIZE (isa_names_table); i++)
	if (isa_names_table[i].option)
	  {
	    if (has_feature (isa_names_table[i].feature))
	      {
		if (codegen_x86_64
		    || (isa_names_table[i].feature != FEATURE_UINTR
			&& isa_names_table[i].feature != FEATURE_APX_F))
		  options = concat (options, " ",
				    isa_names_table[i].option, NULL);
	      }
	    /* Never push -mno-avx10.1-{256,512} under -march=native to
	       avoid unnecessary warnings when building librarys.  */
	    else if (isa_names_table[i].feature != FEATURE_AVX10_1_256
		     && isa_names_table[i].feature != FEATURE_AVX10_1_512
		     && check_avx512_features (cpu_model, cpu_features2,
					       isa_names_table[i].feature))
	      options = concat (options, neg_option,
				isa_names_table[i].option + 2, NULL);
	  }
    }

done:
  return concat (cache, "-m", argv[0], "=", cpu, options, NULL);
}
#else

/* If we are compiling with GCC where %EBX register is fixed, then the
   driver will just ignore -march and -mtune "native" target and will leave
   to the newly built compiler to generate code for its default target.  */

const char *host_detect_local_cpu (int, const char **)
{
  return NULL;
}
#endif /* __GNUC__ */
