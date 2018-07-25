/* Subroutines for the gcc driver.
   Copyright (C) 2006-2018 Free Software Foundation, Inc.

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

const char *host_detect_local_cpu (int argc, const char **argv);

#if defined(__GNUC__) && (__GNUC__ >= 5 || !defined(__PIC__))
#include "cpuid.h"

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

/* This will be called by the spec parser in gcc.c when it sees
   a %:local_cpu_detect(args) construct.  Currently it will be called
   with either "arch" or "tune" as argument depending on if -march=native
   or -mtune=native is to be substituted.

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

  unsigned int eax, ebx, ecx, edx;

  unsigned int max_level, ext_level;

  unsigned int vendor;
  unsigned int model, family;

  unsigned int has_sse3, has_ssse3, has_cmpxchg16b;
  unsigned int has_cmpxchg8b, has_cmov, has_mmx, has_sse, has_sse2;

  /* Extended features */
  unsigned int has_lahf_lm = 0, has_sse4a = 0;
  unsigned int has_longmode = 0, has_3dnowp = 0, has_3dnow = 0;
  unsigned int has_movbe = 0, has_sse4_1 = 0, has_sse4_2 = 0;
  unsigned int has_popcnt = 0, has_aes = 0, has_avx = 0, has_avx2 = 0;
  unsigned int has_pclmul = 0, has_abm = 0, has_lwp = 0;
  unsigned int has_fma = 0, has_fma4 = 0, has_xop = 0;
  unsigned int has_bmi = 0, has_bmi2 = 0, has_tbm = 0, has_lzcnt = 0;
  unsigned int has_hle = 0, has_rtm = 0, has_sgx = 0;
  unsigned int has_pconfig = 0, has_wbnoinvd = 0;
  unsigned int has_rdrnd = 0, has_f16c = 0, has_fsgsbase = 0;
  unsigned int has_rdseed = 0, has_prfchw = 0, has_adx = 0;
  unsigned int has_osxsave = 0, has_fxsr = 0, has_xsave = 0, has_xsaveopt = 0;
  unsigned int has_avx512er = 0, has_avx512pf = 0, has_avx512cd = 0;
  unsigned int has_avx512f = 0, has_sha = 0, has_prefetchwt1 = 0;
  unsigned int has_clflushopt = 0, has_xsavec = 0, has_xsaves = 0;
  unsigned int has_avx512dq = 0, has_avx512bw = 0, has_avx512vl = 0;
  unsigned int has_avx512vbmi = 0, has_avx512ifma = 0, has_clwb = 0;
  unsigned int has_mwaitx = 0, has_clzero = 0, has_pku = 0, has_rdpid = 0;
  unsigned int has_avx5124fmaps = 0, has_avx5124vnniw = 0;
  unsigned int has_gfni = 0, has_avx512vbmi2 = 0;
  unsigned int has_avx512bitalg = 0;
  unsigned int has_shstk = 0;
  unsigned int has_avx512vnni = 0, has_vaes = 0;
  unsigned int has_vpclmulqdq = 0;
  unsigned int has_movdiri = 0, has_movdir64b = 0;
  unsigned int has_waitpkg = 0;
  unsigned int has_cldemote = 0;

  bool arch;

  unsigned int l2sizekb = 0;

  if (argc < 1)
    return NULL;

  arch = !strcmp (argv[0], "arch");

  if (!arch && strcmp (argv[0], "tune"))
    return NULL;

  max_level = __get_cpuid_max (0, &vendor);
  if (max_level < 1)
    goto done;

  __cpuid (1, eax, ebx, ecx, edx);

  model = (eax >> 4) & 0x0f;
  family = (eax >> 8) & 0x0f;
  if (vendor == signature_INTEL_ebx
      || vendor == signature_AMD_ebx)
    {
      unsigned int extended_model, extended_family;

      extended_model = (eax >> 12) & 0xf0;
      extended_family = (eax >> 20) & 0xff;
      if (family == 0x0f)
	{
	  family += extended_family;
	  model += extended_model;
	}
      else if (family == 0x06)
	model += extended_model;
    }

  has_sse3 = ecx & bit_SSE3;
  has_ssse3 = ecx & bit_SSSE3;
  has_sse4_1 = ecx & bit_SSE4_1;
  has_sse4_2 = ecx & bit_SSE4_2;
  has_avx = ecx & bit_AVX;
  has_osxsave = ecx & bit_OSXSAVE;
  has_cmpxchg16b = ecx & bit_CMPXCHG16B;
  has_movbe = ecx & bit_MOVBE;
  has_popcnt = ecx & bit_POPCNT;
  has_aes = ecx & bit_AES;
  has_pclmul = ecx & bit_PCLMUL;
  has_fma = ecx & bit_FMA;
  has_f16c = ecx & bit_F16C;
  has_rdrnd = ecx & bit_RDRND;
  has_xsave = ecx & bit_XSAVE;

  has_cmpxchg8b = edx & bit_CMPXCHG8B;
  has_cmov = edx & bit_CMOV;
  has_mmx = edx & bit_MMX;
  has_fxsr = edx & bit_FXSAVE;
  has_sse = edx & bit_SSE;
  has_sse2 = edx & bit_SSE2;

  if (max_level >= 7)
    {
      __cpuid_count (7, 0, eax, ebx, ecx, edx);

      has_bmi = ebx & bit_BMI;
      has_sgx = ebx & bit_SGX;
      has_hle = ebx & bit_HLE;
      has_rtm = ebx & bit_RTM;
      has_avx2 = ebx & bit_AVX2;
      has_bmi2 = ebx & bit_BMI2;
      has_fsgsbase = ebx & bit_FSGSBASE;
      has_rdseed = ebx & bit_RDSEED;
      has_adx = ebx & bit_ADX;
      has_avx512f = ebx & bit_AVX512F;
      has_avx512er = ebx & bit_AVX512ER;
      has_avx512pf = ebx & bit_AVX512PF;
      has_avx512cd = ebx & bit_AVX512CD;
      has_sha = ebx & bit_SHA;
      has_clflushopt = ebx & bit_CLFLUSHOPT;
      has_clwb = ebx & bit_CLWB;
      has_avx512dq = ebx & bit_AVX512DQ;
      has_avx512bw = ebx & bit_AVX512BW;
      has_avx512vl = ebx & bit_AVX512VL;
      has_avx512ifma = ebx & bit_AVX512IFMA;

      has_prefetchwt1 = ecx & bit_PREFETCHWT1;
      has_avx512vbmi = ecx & bit_AVX512VBMI;
      has_pku = ecx & bit_OSPKE;
      has_avx512vbmi2 = ecx & bit_AVX512VBMI2;
      has_avx512vnni = ecx & bit_AVX512VNNI;
      has_rdpid = ecx & bit_RDPID;
      has_gfni = ecx & bit_GFNI;
      has_vaes = ecx & bit_VAES;
      has_vpclmulqdq = ecx & bit_VPCLMULQDQ;
      has_avx512bitalg = ecx & bit_AVX512BITALG;
      has_movdiri = ecx & bit_MOVDIRI;
      has_movdir64b = ecx & bit_MOVDIR64B;
      has_cldemote = ecx & bit_CLDEMOTE;

      has_avx5124vnniw = edx & bit_AVX5124VNNIW;
      has_avx5124fmaps = edx & bit_AVX5124FMAPS;

      has_shstk = ecx & bit_SHSTK;
      has_pconfig = edx & bit_PCONFIG;
      has_waitpkg = ecx & bit_WAITPKG;
    }

  if (max_level >= 13)
    {
      __cpuid_count (13, 1, eax, ebx, ecx, edx);

      has_xsaveopt = eax & bit_XSAVEOPT;
      has_xsavec = eax & bit_XSAVEC;
      has_xsaves = eax & bit_XSAVES;
    }

  /* Check cpuid level of extended features.  */
  __cpuid (0x80000000, ext_level, ebx, ecx, edx);

  if (ext_level >= 0x80000001)
    {
      __cpuid (0x80000001, eax, ebx, ecx, edx);

      has_lahf_lm = ecx & bit_LAHF_LM;
      has_sse4a = ecx & bit_SSE4a;
      has_abm = ecx & bit_ABM;
      has_lwp = ecx & bit_LWP;
      has_fma4 = ecx & bit_FMA4;
      has_xop = ecx & bit_XOP;
      has_tbm = ecx & bit_TBM;
      has_lzcnt = ecx & bit_LZCNT;
      has_prfchw = ecx & bit_PRFCHW;

      has_longmode = edx & bit_LM;
      has_3dnowp = edx & bit_3DNOWP;
      has_3dnow = edx & bit_3DNOW;
      has_mwaitx = ecx & bit_MWAITX;
    }

  if (ext_level >= 0x80000008)
    {
      __cpuid (0x80000008, eax, ebx, ecx, edx);
      has_clzero = ebx & bit_CLZERO;
      has_wbnoinvd = ebx & bit_WBNOINVD;
    }

  /* Get XCR_XFEATURE_ENABLED_MASK register with xgetbv.  */
#define XCR_XFEATURE_ENABLED_MASK	0x0
#define XSTATE_FP			0x1
#define XSTATE_SSE			0x2
#define XSTATE_YMM			0x4
#define XSTATE_OPMASK			0x20
#define XSTATE_ZMM			0x40
#define XSTATE_HI_ZMM			0x80

#define XCR_AVX_ENABLED_MASK \
  (XSTATE_SSE | XSTATE_YMM)
#define XCR_AVX512F_ENABLED_MASK \
  (XSTATE_SSE | XSTATE_YMM | XSTATE_OPMASK | XSTATE_ZMM | XSTATE_HI_ZMM)

  if (has_osxsave)
    asm (".byte 0x0f; .byte 0x01; .byte 0xd0"
	 : "=a" (eax), "=d" (edx)
	 : "c" (XCR_XFEATURE_ENABLED_MASK));
  else
    eax = 0;

  /* Check if AVX registers are supported.  */
  if ((eax & XCR_AVX_ENABLED_MASK) != XCR_AVX_ENABLED_MASK)
    {
      has_avx = 0;
      has_avx2 = 0;
      has_fma = 0;
      has_fma4 = 0;
      has_f16c = 0;
      has_xop = 0;
      has_xsave = 0;
      has_xsaveopt = 0;
      has_xsaves = 0;
      has_xsavec = 0;
    }

  /* Check if AVX512F registers are supported.  */
  if ((eax & XCR_AVX512F_ENABLED_MASK) != XCR_AVX512F_ENABLED_MASK)
    {
      has_avx512f = 0;
      has_avx512er = 0;
      has_avx512pf = 0;
      has_avx512cd = 0;
      has_avx512dq = 0;
      has_avx512bw = 0;
      has_avx512vl = 0;
    }

  if (!arch)
    {
      if (vendor == signature_AMD_ebx
	  || vendor == signature_CENTAUR_ebx
	  || vendor == signature_CYRIX_ebx
	  || vendor == signature_NSC_ebx)
	cache = detect_caches_amd (ext_level);
      else if (vendor == signature_INTEL_ebx)
	{
	  bool xeon_mp = (family == 15 && model == 6);
	  cache = detect_caches_intel (xeon_mp, max_level,
				       ext_level, &l2sizekb);
	}
    }

  if (vendor == signature_AMD_ebx)
    {
      unsigned int name;

      /* Detect geode processor by its processor signature.  */
      if (ext_level >= 0x80000002)
	__cpuid (0x80000002, name, ebx, ecx, edx);
      else
	name = 0;

      if (name == signature_NSC_ebx)
	processor = PROCESSOR_GEODE;
      else if (has_movbe && family == 22)
	processor = PROCESSOR_BTVER2;
      else if (has_clzero)
	processor = PROCESSOR_ZNVER1;
      else if (has_avx2)
        processor = PROCESSOR_BDVER4;
      else if (has_xsaveopt)
        processor = PROCESSOR_BDVER3;
      else if (has_bmi)
        processor = PROCESSOR_BDVER2;
      else if (has_xop)
	processor = PROCESSOR_BDVER1;
      else if (has_sse4a && has_ssse3)
        processor = PROCESSOR_BTVER1;
      else if (has_sse4a)
	processor = PROCESSOR_AMDFAM10;
      else if (has_sse2 || has_longmode)
	processor = PROCESSOR_K8;
      else if (has_3dnowp && family == 6)
	processor = PROCESSOR_ATHLON;
      else if (has_mmx)
	processor = PROCESSOR_K6;
      else
	processor = PROCESSOR_PENTIUM;
    }
  else if (vendor == signature_CENTAUR_ebx)
    {
      processor = PROCESSOR_GENERIC;

      switch (family)
	{
	default:
	  /* We have no idea.  */
	  break;

	case 5:
	  if (has_3dnow || has_mmx)
	    processor = PROCESSOR_I486;
	  break;

	case 6:
	  if (has_longmode)
	    processor = PROCESSOR_K8;
	  else if (model >= 9)
	    processor = PROCESSOR_PENTIUMPRO;
	  else if (model >= 6)
	    processor = PROCESSOR_I486;
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
      if (arch && vendor == signature_CENTAUR_ebx)
	{
	  if (model >= 6)
	    cpu = "c3";
	  else if (has_3dnow)
	    cpu = "winchip2";
	  else
	    /* Assume WinChip C6.  */
	    cpu = "winchip-c6";
	}
      else
	cpu = "i486";
      break;
    case PROCESSOR_PENTIUM:
      if (arch && has_mmx)
	cpu = "pentium-mmx";
      else
	cpu = "pentium";
      break;
    case PROCESSOR_PENTIUMPRO:
      switch (model)
	{
	case 0x1c:
	case 0x26:
	  /* Bonnell.  */
	  cpu = "bonnell";
	  break;
	case 0x37:
	case 0x4a:
	case 0x4d:
	case 0x5a:
	case 0x5d:
	  /* Silvermont.  */
	  cpu = "silvermont";
	  break;
	case 0x5c:
	case 0x5f:
	  /* Goldmont.  */
	  cpu = "goldmont";
	  break;
	case 0x7a:
	  /* Goldmont Plus.  */
	  cpu = "goldmont-plus";
	  break;
	case 0x0f:
	  /* Merom.  */
	case 0x17:
	case 0x1d:
	  /* Penryn.  */
	  cpu = "core2";
	  break;
	case 0x1a:
	case 0x1e:
	case 0x1f:
	case 0x2e:
	  /* Nehalem.  */
	  cpu = "nehalem";
	  break;
	case 0x25:
	case 0x2c:
	case 0x2f:
	  /* Westmere.  */
	  cpu = "westmere";
	  break;
	case 0x2a:
	case 0x2d:
	  /* Sandy Bridge.  */
	  cpu = "sandybridge";
	  break;
	case 0x3a:
	case 0x3e:
	  /* Ivy Bridge.  */
	  cpu = "ivybridge";
	  break;
	case 0x3c:
	case 0x3f:
	case 0x45:
	case 0x46:
	  /* Haswell.  */
	  cpu = "haswell";
	  break;
	case 0x3d:
	case 0x47:
	case 0x4f:
	case 0x56:
	  /* Broadwell.  */
	  cpu = "broadwell";
	  break;
	case 0x4e:
	case 0x5e:
	  /* Skylake.  */
	case 0x8e:
	case 0x9e:
	  /* Kaby Lake.  */
	  cpu = "skylake";
	  break;
	case 0x55:
	  /* Skylake with AVX-512.  */
	  cpu = "skylake-avx512";
	  break;
	case 0x57:
	  /* Knights Landing.  */
	  cpu = "knl";
	  break;
	case 0x66:
	  /* Cannon Lake.  */
	  cpu = "cannonlake";
	  break;
	case 0x85:
	  /* Knights Mill.  */
	  cpu = "knm";
	  break;
	default:
	  if (arch)
	    {
	      /* This is unknown family 0x6 CPU.  */
	      /* Assume Ice Lake Server.  */
	      if (has_wbnoinvd)
		cpu = "icelake-server";
	      /* Assume Ice Lake.  */
	      else if (has_gfni)
		cpu = "icelake-client";
	      /* Assume Cannon Lake.  */
	      else if (has_avx512vbmi)
		cpu = "cannonlake";
	      /* Assume Knights Mill.  */
	      else if (has_avx5124vnniw)
		cpu = "knm";
	      /* Assume Knights Landing.  */
	      else if (has_avx512er)
		cpu = "knl";
	      /* Assume Skylake with AVX-512.  */
	      else if (has_avx512f)
		cpu = "skylake-avx512";
	      /* Assume Skylake.  */
	      else if (has_clflushopt)
		cpu = "skylake";
	      /* Assume Broadwell.  */
	      else if (has_adx)
		cpu = "broadwell";
	      else if (has_avx2)
		/* Assume Haswell.  */
		cpu = "haswell";
	      else if (has_avx)
		/* Assume Sandy Bridge.  */
		cpu = "sandybridge";
	      else if (has_sse4_2)
		{
		  if (has_gfni)
		    /* Assume Tremont.  */
		    cpu = "tremont";
		  else if (has_sgx)
		    /* Assume Goldmont Plus.  */
		    cpu = "goldmont-plus";
		  else if (has_xsave)
		    /* Assume Goldmont.  */
		    cpu = "goldmont";
		  else if (has_movbe)
		    /* Assume Silvermont.  */
		    cpu = "silvermont";
		  else
		    /* Assume Nehalem.  */
		    cpu = "nehalem";
		}
	      else if (has_ssse3)
		{
		  if (has_movbe)
		    /* Assume Bonnell.  */
		    cpu = "bonnell";
		  else
		    /* Assume Core 2.  */
		    cpu = "core2";
		}
	      else if (has_longmode)
		/* Perhaps some emulator?  Assume x86-64, otherwise gcc
		   -march=native would be unusable for 64-bit compilations,
		   as all the CPUs below are 32-bit only.  */
		cpu = "x86-64";
	      else if (has_sse3)
		{
		  if (vendor == signature_CENTAUR_ebx)
		    /* C7 / Eden "Esther" */
		    cpu = "c7";
		  else
		    /* It is Core Duo.  */
		    cpu = "pentium-m";
		}
	      else if (has_sse2)
		/* It is Pentium M.  */
		cpu = "pentium-m";
	      else if (has_sse)
		{
		  if (vendor == signature_CENTAUR_ebx)
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
	      else if (has_mmx)
		/* It is Pentium II.  */
		cpu = "pentium2";
	      else
		/* Default to Pentium Pro.  */
		cpu = "pentiumpro";
	    }
	  else
	    /* For -mtune, we default to -mtune=generic.  */
	    cpu = "generic";
	  break;
	}
      break;
    case PROCESSOR_PENTIUM4:
      if (has_sse3)
	{
	  if (has_longmode)
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
      if (arch && has_3dnow)
	cpu = "k6-3";
      else
	cpu = "k6";
      break;
    case PROCESSOR_ATHLON:
      if (arch && has_sse)
	cpu = "athlon-4";
      else
	cpu = "athlon";
      break;
    case PROCESSOR_K8:
      if (arch)
	{
	  if (vendor == signature_CENTAUR_ebx)
	    {
	      if (has_sse4_1)
		/* Nano 3000 | Nano dual / quad core | Eden X4 */
		cpu = "nano-3000";
	      else if (has_ssse3)
		/* Nano 1000 | Nano 2000 */
		cpu = "nano";
	      else if (has_sse3)
		/* Eden X2 */
		cpu = "eden-x2";
	      else
		/* Default to k8 */
		cpu = "k8";
	    }
	  else if (has_sse3)
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
    case PROCESSOR_BTVER1:
      cpu = "btver1";
      break;
    case PROCESSOR_BTVER2:
      cpu = "btver2";
      break;

    default:
      /* Use something reasonable.  */
      if (arch)
	{
	  if (has_ssse3)
	    cpu = "core2";
	  else if (has_sse3)
	    {
	      if (has_longmode)
		cpu = "nocona";
	      else
		cpu = "prescott";
	    }
	  else if (has_longmode)
	    /* Perhaps some emulator?  Assume x86-64, otherwise gcc
	       -march=native would be unusable for 64-bit compilations,
	       as all the CPUs below are 32-bit only.  */
	    cpu = "x86-64";
	  else if (has_sse2)
	    cpu = "pentium4";
	  else if (has_cmov)
	    cpu = "pentiumpro";
	  else if (has_mmx)
	    cpu = "pentium-mmx";
	  else if (has_cmpxchg8b)
	    cpu = "pentium";
	}
      else
	cpu = "generic";
    }

  if (arch)
    {
      const char *mmx = has_mmx ? " -mmmx" : " -mno-mmx";
      const char *mmx3dnow = has_3dnow ? " -m3dnow" : " -mno-3dnow";
      const char *sse = has_sse ? " -msse" : " -mno-sse";
      const char *sse2 = has_sse2 ? " -msse2" : " -mno-sse2";
      const char *sse3 = has_sse3 ? " -msse3" : " -mno-sse3";
      const char *ssse3 = has_ssse3 ? " -mssse3" : " -mno-ssse3";
      const char *sse4a = has_sse4a ? " -msse4a" : " -mno-sse4a";
      const char *cx16 = has_cmpxchg16b ? " -mcx16" : " -mno-cx16";
      const char *sahf = has_lahf_lm ? " -msahf" : " -mno-sahf";
      const char *movbe = has_movbe ? " -mmovbe" : " -mno-movbe";
      const char *aes = has_aes ? " -maes" : " -mno-aes";
      const char *sha = has_sha ? " -msha" : " -mno-sha";
      const char *pclmul = has_pclmul ? " -mpclmul" : " -mno-pclmul";
      const char *popcnt = has_popcnt ? " -mpopcnt" : " -mno-popcnt";
      const char *abm = has_abm ? " -mabm" : " -mno-abm";
      const char *lwp = has_lwp ? " -mlwp" : " -mno-lwp";
      const char *fma = has_fma ? " -mfma" : " -mno-fma";
      const char *fma4 = has_fma4 ? " -mfma4" : " -mno-fma4";
      const char *xop = has_xop ? " -mxop" : " -mno-xop";
      const char *bmi = has_bmi ? " -mbmi" : " -mno-bmi";
      const char *pconfig = has_pconfig ? " -mpconfig" : " -mno-pconfig";
      const char *wbnoinvd = has_wbnoinvd ? " -mwbnoinvd" : " -mno-wbnoinvd";
      const char *sgx = has_sgx ? " -msgx" : " -mno-sgx";
      const char *bmi2 = has_bmi2 ? " -mbmi2" : " -mno-bmi2";
      const char *tbm = has_tbm ? " -mtbm" : " -mno-tbm";
      const char *avx = has_avx ? " -mavx" : " -mno-avx";
      const char *avx2 = has_avx2 ? " -mavx2" : " -mno-avx2";
      const char *sse4_2 = has_sse4_2 ? " -msse4.2" : " -mno-sse4.2";
      const char *sse4_1 = has_sse4_1 ? " -msse4.1" : " -mno-sse4.1";
      const char *lzcnt = has_lzcnt ? " -mlzcnt" : " -mno-lzcnt";
      const char *hle = has_hle ? " -mhle" : " -mno-hle";
      const char *rtm = has_rtm ? " -mrtm" : " -mno-rtm";
      const char *rdrnd = has_rdrnd ? " -mrdrnd" : " -mno-rdrnd";
      const char *f16c = has_f16c ? " -mf16c" : " -mno-f16c";
      const char *fsgsbase = has_fsgsbase ? " -mfsgsbase" : " -mno-fsgsbase";
      const char *rdseed = has_rdseed ? " -mrdseed" : " -mno-rdseed";
      const char *prfchw = has_prfchw ? " -mprfchw" : " -mno-prfchw";
      const char *adx = has_adx ? " -madx" : " -mno-adx";
      const char *fxsr = has_fxsr ? " -mfxsr" : " -mno-fxsr";
      const char *xsave = has_xsave ? " -mxsave" : " -mno-xsave";
      const char *xsaveopt = has_xsaveopt ? " -mxsaveopt" : " -mno-xsaveopt";
      const char *avx512f = has_avx512f ? " -mavx512f" : " -mno-avx512f";
      const char *avx512er = has_avx512er ? " -mavx512er" : " -mno-avx512er";
      const char *avx512cd = has_avx512cd ? " -mavx512cd" : " -mno-avx512cd";
      const char *avx512pf = has_avx512pf ? " -mavx512pf" : " -mno-avx512pf";
      const char *prefetchwt1 = has_prefetchwt1 ? " -mprefetchwt1" : " -mno-prefetchwt1";
      const char *clflushopt = has_clflushopt ? " -mclflushopt" : " -mno-clflushopt";
      const char *xsavec = has_xsavec ? " -mxsavec" : " -mno-xsavec";
      const char *xsaves = has_xsaves ? " -mxsaves" : " -mno-xsaves";
      const char *avx512dq = has_avx512dq ? " -mavx512dq" : " -mno-avx512dq";
      const char *avx512bw = has_avx512bw ? " -mavx512bw" : " -mno-avx512bw";
      const char *avx512vl = has_avx512vl ? " -mavx512vl" : " -mno-avx512vl";
      const char *avx512ifma = has_avx512ifma ? " -mavx512ifma" : " -mno-avx512ifma";
      const char *avx512vbmi = has_avx512vbmi ? " -mavx512vbmi" : " -mno-avx512vbmi";
      const char *avx5124vnniw = has_avx5124vnniw ? " -mavx5124vnniw" : " -mno-avx5124vnniw";
      const char *avx512vbmi2 = has_avx512vbmi2 ? " -mavx512vbmi2" : " -mno-avx512vbmi2";
      const char *avx512vnni = has_avx512vnni ? " -mavx512vnni" : " -mno-avx512vnni";
      const char *avx5124fmaps = has_avx5124fmaps ? " -mavx5124fmaps" : " -mno-avx5124fmaps";
      const char *clwb = has_clwb ? " -mclwb" : " -mno-clwb";
      const char *mwaitx  = has_mwaitx  ? " -mmwaitx"  : " -mno-mwaitx"; 
      const char *clzero  = has_clzero  ? " -mclzero"  : " -mno-clzero";
      const char *pku = has_pku ? " -mpku" : " -mno-pku";
      const char *rdpid = has_rdpid ? " -mrdpid" : " -mno-rdpid";
      const char *gfni = has_gfni ? " -mgfni" : " -mno-gfni";
      const char *shstk = has_shstk ? " -mshstk" : " -mno-shstk";
      const char *vaes = has_vaes ? " -mvaes" : " -mno-vaes";
      const char *vpclmulqdq = has_vpclmulqdq ? " -mvpclmulqdq" : " -mno-vpclmulqdq";
      const char *avx512bitalg = has_avx512bitalg ? " -mavx512bitalg" : " -mno-avx512bitalg";
      const char *movdiri = has_movdiri ? " -mmovdiri" : " -mno-movdiri";
      const char *movdir64b = has_movdir64b ? " -mmovdir64b" : " -mno-movdir64b";
      const char *waitpkg = has_waitpkg ? " -mwaitpkg" : " -mno-waitpkg";
      const char *cldemote = has_cldemote ? " -mcldemote" : " -mno-cldemote";
      options = concat (options, mmx, mmx3dnow, sse, sse2, sse3, ssse3,
			sse4a, cx16, sahf, movbe, aes, sha, pclmul,
			popcnt, abm, lwp, fma, fma4, xop, bmi, sgx, bmi2,
			pconfig, wbnoinvd,
			tbm, avx, avx2, sse4_2, sse4_1, lzcnt, rtm,
			hle, rdrnd, f16c, fsgsbase, rdseed, prfchw, adx,
			fxsr, xsave, xsaveopt, avx512f, avx512er,
			avx512cd, avx512pf, prefetchwt1, clflushopt,
			xsavec, xsaves, avx512dq, avx512bw, avx512vl,
			avx512ifma, avx512vbmi, avx5124fmaps, avx5124vnniw,
			clwb, mwaitx, clzero, pku, rdpid, gfni, shstk,
			avx512vbmi2, avx512vnni, vaes, vpclmulqdq,
			avx512bitalg, movdiri, movdir64b, waitpkg, cldemote,
			NULL);
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
