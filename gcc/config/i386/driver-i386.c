/* Subroutines for the gcc driver.
   Copyright (C) 2006, 2007, 2009 Free Software Foundation, Inc.

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

/* Returns parameters that describe L1_ASSOC associative cache of size
   L1_SIZEKB with lines of size L1_LINE.  */

static char *
describe_cache (unsigned l1_sizekb, unsigned l1_line,
		unsigned l1_assoc ATTRIBUTE_UNUSED)
{
  char size[100], line[100];

  /* At the moment, gcc middle-end does not use the information about the
     associativity of the cache.  */

  sprintf (size, "--param l1-cache-size=%u", l1_sizekb);
  sprintf (line, "--param l1-cache-line-size=%u", l1_line);

  return concat (size, " ", line, " ", NULL);
}

/* Returns the description of caches for an AMD processor.  */

static char *
detect_caches_amd (unsigned max_ext_level)
{
  unsigned eax, ebx, ecx, edx;
  unsigned l1_sizekb, l1_line, l1_assoc;

  if (max_ext_level < 0x80000005)
    return (char *) "";

  __cpuid (0x80000005, eax, ebx, ecx, edx);

  l1_line = ecx & 0xff;
  l1_sizekb = (ecx >> 24) & 0xff;
  l1_assoc = (ecx >> 16) & 0xff;

  return describe_cache (l1_sizekb, l1_line, l1_assoc);
}

/* Stores the size of the L1 cache and cache line, and the associativity
   of the cache according to REG to L1_SIZEKB, L1_LINE and L1_ASSOC.  */

static void
decode_caches_intel (unsigned reg, unsigned *l1_sizekb, unsigned *l1_line,
		     unsigned *l1_assoc)
{
  unsigned i, val;

  if (((reg >> 31) & 1) != 0)
    return;

  for (i = 0; i < 4; i++)
    {
      val = reg & 0xff;
      reg >>= 8;

      switch (val)
	{
	case 0xa:
	  *l1_sizekb = 8;
	  *l1_line = 32;
	  *l1_assoc = 2;
	  break;
	case 0xc:
	  *l1_sizekb = 16;
	  *l1_line = 32;
	  *l1_assoc = 4;
	  break;
	case 0x2c:
	  *l1_sizekb = 32;
	  *l1_line = 64;
	  *l1_assoc = 8;
	  break;
	case 0x60:
	  *l1_sizekb = 16;
	  *l1_line = 64;
	  *l1_assoc = 8;
	  break;
	case 0x66:
	  *l1_sizekb = 8;
	  *l1_line = 64;
	  *l1_assoc = 4;
	  break;
	case 0x67:
	  *l1_sizekb = 16;
	  *l1_line = 64;
	  *l1_assoc = 4;
	  break;
	case 0x68:
	  *l1_sizekb = 32;
	  *l1_line = 64;
	  *l1_assoc = 4;
	  break;

	default:
	  break;
	}
    }
}

/* Returns the description of caches for an intel processor.  */

static char *
detect_caches_intel (unsigned max_level)
{
  unsigned eax, ebx, ecx, edx;
  unsigned l1_sizekb = 0, l1_line = 0, assoc = 0;

  if (max_level < 2)
    return (char *) "";

  __cpuid (2, eax, ebx, ecx, edx);

  decode_caches_intel (eax, &l1_sizekb, &l1_line, &assoc);
  decode_caches_intel (ebx, &l1_sizekb, &l1_line, &assoc);
  decode_caches_intel (ecx, &l1_sizekb, &l1_line, &assoc);
  decode_caches_intel (edx, &l1_sizekb, &l1_line, &assoc);

  if (!l1_sizekb)
    return (char *) "";

  return describe_cache (l1_sizekb, l1_line, assoc);
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
  unsigned int family;

  unsigned int has_sse3, has_ssse3, has_cmpxchg16b;
  unsigned int has_cmpxchg8b, has_cmov, has_mmx, has_sse, has_sse2;

  /* Extended features */
  unsigned int has_lahf_lm = 0, has_sse4a = 0;
  unsigned int has_longmode = 0, has_3dnowp = 0, has_3dnow = 0;

  bool arch;

  if (argc < 1)
    return NULL;

  arch = !strcmp (argv[0], "arch");

  if (!arch && strcmp (argv[0], "tune"))
    return NULL;

  max_level = __get_cpuid_max (0, &vendor);
  if (max_level < 1)
    goto done;

  __cpuid (1, eax, ebx, ecx, edx);

  /* We don't care for extended family.  */
  family = (eax >> 8) & 0x0f;

  has_sse3 = ecx & bit_SSE3;
  has_ssse3 = ecx & bit_SSSE3;
  has_cmpxchg16b = ecx & bit_CMPXCHG16B;

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

      has_longmode = edx & bit_LM;
      has_3dnowp = edx & bit_3DNOWP;
      has_3dnow = edx & bit_3DNOW;
    }

  if (!arch)
    {
      if (vendor == SIG_AMD)
	cache = detect_caches_amd (ext_level);
      else if (vendor == SIG_INTEL)
	cache = detect_caches_intel (max_level);
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
      else if (has_3dnowp)
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
      if (has_longmode)
	/* It is Core 2 Duo.  */
	cpu = "core2";
      else if (arch)
	{
	  if (has_sse3)
	    /* It is Core Duo.  */
	    cpu = "prescott";
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
	options = concat (options, "-mcx16 ", NULL);
      if (has_lahf_lm)
	options = concat (options, "-msahf ", NULL);
    }

done:
  return concat (cache, "-m", argv[0], "=", cpu, " ", options, NULL);
}
#else

/* If we aren't compiling with GCC we just provide a minimal
   default value.  */

const char *host_detect_local_cpu (int argc, const char **argv)
{
  const char *cpu;
  bool arch;

  if (argc < 1)
    return NULL;

  arch = !strcmp (argv[0], "arch");

  if (!arch && strcmp (argv[0], "tune"))
    return NULL;
  
  if (arch)
    {
      /* FIXME: i386 is wrong for 64bit compiler.  How can we tell if
	 we are generating 64bit or 32bit code?  */
      cpu = "i386";
    }
  else
    cpu = "generic";

  return concat ("-m", argv[0], "=", cpu, NULL);
}
#endif /* __GNUC__ */
