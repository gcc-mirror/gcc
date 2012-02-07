/* Subroutines for the gcc driver.
   Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.

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
#include <stdlib.h>

const char *host_detect_local_cpu (int argc, const char **argv);

#ifdef __GNUC__
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

enum vendor_signatures
{
  SIG_INTEL =	0x756e6547 /* Genu */,
  SIG_AMD =	0x68747541 /* Auth */
};

enum processor_signatures
{
  SIG_GEODE =	0x646f6547 /* Geod */
};

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
  unsigned int has_popcnt = 0, has_aes = 0, has_avx = 0;
  unsigned int has_pclmul = 0, has_abm = 0, has_lwp = 0;

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
  if (vendor == SIG_INTEL)
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
  has_cmpxchg16b = ecx & bit_CMPXCHG16B;
  has_movbe = ecx & bit_MOVBE;
  has_popcnt = ecx & bit_POPCNT;
  has_aes = ecx & bit_AES;
  has_pclmul = ecx & bit_PCLMUL;

  has_cmpxchg8b = edx & bit_CMPXCHG8B;
  has_cmov = edx & bit_CMOV;
  has_mmx = edx & bit_MMX;
  has_sse = edx & bit_SSE;
  has_sse2 = edx & bit_SSE2;

  /* Check cpuid level of extended features.  */
  __cpuid (0x80000000, ext_level, ebx, ecx, edx);

  if (ext_level > 0x80000000)
    {
      __cpuid (0x80000001, eax, ebx, ecx, edx);

      has_lahf_lm = ecx & bit_LAHF_LM;
      has_sse4a = ecx & bit_SSE4a;
      has_abm = ecx & bit_ABM;
      has_lwp = ecx & bit_LWP;

      has_longmode = edx & bit_LM;
      has_3dnowp = edx & bit_3DNOWP;
      has_3dnow = edx & bit_3DNOW;
    }

  if (!arch)
    {
      if (vendor == SIG_AMD)
	cache = detect_caches_amd (ext_level);
      else if (vendor == SIG_INTEL)
	{
	  bool xeon_mp = (family == 15 && model == 6);
	  cache = detect_caches_intel (xeon_mp, max_level,
				       ext_level, &l2sizekb);
	}
    }

  if (vendor == SIG_AMD)
    {
      unsigned int name;

      /* Detect geode processor by its processor signature.  */
      if (ext_level > 0x80000001)
	__cpuid (0x80000002, name, ebx, ecx, edx);
      else
	name = 0;

      if (name == SIG_GEODE)
	processor = PROCESSOR_GEODE;
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
	  processor = PROCESSOR_GENERIC32;
	}
    }

  switch (processor)
    {
    case PROCESSOR_I386:
      /* Default.  */
      break;
    case PROCESSOR_I486:
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
	  /* Atom.  */
	  cpu = "atom";
	  break;
	case 0x1a:
	case 0x1e:
	case 0x1f:
	case 0x2e:
	  /* FIXME: Optimize for Nehalem.  */
	  cpu = "core2";
	  break;
	case 0x25:
	case 0x2f:
	  /* FIXME: Optimize for Westmere.  */
	  cpu = "core2";
	  break;
	case 0x17:
	case 0x1d:
	  /* Penryn.  FIXME: -mtune=core2 is slower than -mtune=generic  */
	  cpu = "core2";
	  break;
	case 0x0f:
	  /* Merom.  FIXME: -mtune=core2 is slower than -mtune=generic  */
	  cpu = "core2";
	  break;
	default:
	  if (arch)
	    {
	      if (has_ssse3)
		/* If it is an unknown CPU with SSSE3, assume Core 2.  */
		cpu = "core2";
	      else if (has_sse3)
		/* It is Core Duo.  */
		cpu = "pentium-m";
	      else if (has_sse2)
		/* It is Pentium M.  */
		cpu = "pentium-m";
	      else if (has_sse)
		/* It is Pentium III.  */
		cpu = "pentium3";
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
      if (arch && has_sse3)
	cpu = "k8-sse3";
      else
	cpu = "k8";
      break;
    case PROCESSOR_AMDFAM10:
      cpu = "amdfam10";
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
      if (has_cmpxchg16b)
	options = concat (options, " -mcx16", NULL);
      if (has_lahf_lm)
	options = concat (options, " -msahf", NULL);
      if (has_movbe)
	options = concat (options, " -mmovbe", NULL);
      if (has_aes)
	options = concat (options, " -maes", NULL);
      if (has_pclmul)
	options = concat (options, " -mpclmul", NULL);
      if (has_popcnt)
	options = concat (options, " -mpopcnt", NULL);
      if (has_abm)
	options = concat (options, " -mabm", NULL);
      if (has_lwp)
	options = concat (options, " -mlwp", NULL);

      if (has_avx)
	options = concat (options, " -mavx", NULL);
      else if (has_sse4_2)
	options = concat (options, " -msse4.2", NULL);
      else if (has_sse4_1)
	options = concat (options, " -msse4.1", NULL);
    }

done:
  return concat (cache, "-m", argv[0], "=", cpu, options, NULL);
}
#else

/* If we aren't compiling with GCC then the driver will just ignore
   -march and -mtune "native" target and will leave to the newly
   built compiler to generate code for its default target.  */

const char *host_detect_local_cpu (int argc ATTRIBUTE_UNUSED,
				   const char **argv ATTRIBUTE_UNUSED)
{
  return NULL;
}
#endif /* __GNUC__ */
