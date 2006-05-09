/* Subroutines for the gcc driver.
   Copyright (C) 2006 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include <stdlib.h>

#ifndef CROSS_COMPILE
/* This file shouldn't even be included in a cross compiler, but
   let's be sure.  */
extern const char *host_detect_local_cpu (int argc, const char **argv);

#ifdef GCC_VERSION
#define cpuid(num,a,b,c,d) \
  asm volatile ("xchgl %%ebx, %1; cpuid; xchgl %%ebx, %1" \
		: "=a" (a), "=r" (b), "=c" (c), "=d" (d)  \
		: "0" (num))

#define bit_CMPXCHG8B (1 << 8)
#define bit_CMOV (1 << 15)
#define bit_MMX (1 << 23)
#define bit_SSE (1 << 25)
#define bit_SSE2 (1 << 26)

#define bit_SSE3 (1 << 0)
#define bit_CMPXCHG16B (1 << 13)

#define bit_3DNOW (1 << 31)
#define bit_3DNOWP (1 << 30)
#define bit_LM (1 << 29)

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
  const char *cpu = "i386";
  unsigned int eax, ebx, ecx, edx;
  unsigned int max_level;
  unsigned int vendor;
  unsigned int ext_level;
  unsigned char has_mmx = 0, has_3dnow = 0, has_3dnowp = 0, has_sse = 0;
  unsigned char has_sse2 = 0, has_sse3 = 0, has_cmov = 0;
  unsigned char has_longmode = 0;
  unsigned char is_amd = 0;
  unsigned int family = 0;
  if (argc < 1
      || (strcmp (argv[0], "arch")
	  && strcmp (argv[0], "tune")))
    return NULL;

#ifndef __x86_64__
  /* See if we can use cpuid.  */
  asm volatile ("pushfl; pushfl; popl %0; movl %0,%1; xorl %2,%0;"
		"pushl %0; popfl; pushfl; popl %0; popfl"
		: "=&r" (eax), "=&r" (ebx)
		: "i" (0x00200000));

  if (((eax ^ ebx) & 0x00200000) == 0)
    goto done;
#endif

  cpu = "i586";

  /* Check the highest input value for eax.  */
  cpuid (0, eax, ebx, ecx, edx);
  max_level = eax;
  /* We only look at the first four characters.  */
  vendor = ebx;
  if (max_level == 0)
    goto done;

  cpuid (1, eax, ebx, ecx, edx);
  has_cmov = !!(edx & bit_CMOV);
  has_mmx = !!(edx & bit_MMX);
  has_sse = !!(edx & bit_SSE);
  has_sse2 = !!(edx & bit_SSE2);
  has_sse3 = !!(ecx & bit_SSE3);
  /* We don't care for extended family.  */
  family = (eax >> 8) & ~(1 << 4);

  cpuid (0x80000000, eax, ebx, ecx, edx);
  ext_level = eax;
  if (ext_level >= 0x80000000)
    {
      cpuid (0x80000001, eax, ebx, ecx, edx);
      has_3dnow = !!(edx & bit_3DNOW);
      has_3dnowp = !!(edx & bit_3DNOWP);
      has_longmode = !!(edx & bit_LM);
    }

  is_amd = vendor == *(unsigned int*)"Auth";

  if (is_amd)
    {
      if (has_mmx)
        cpu = "k6";
      if (has_3dnow)
        cpu = "k6-3";
      if (has_3dnowp)
        cpu = "athlon";
      if (has_sse)
	cpu = "athlon-4";
      if (has_sse2 || has_longmode)
        cpu = "k8";
    }
  else
    {
      if (family == 5)
        {
          if (has_mmx)
            cpu = "pentium-mmx";
	}
      else if (has_mmx)
        cpu = "pentium2";
      if (has_sse)
        cpu = "pentium3";
      if (has_sse2)
	{
	  if (family == 6)
	    /* It's a pentiumpro with sse2 --> pentium-m */
            cpu = "pentium-m";
	  else
	    /* Would have to look at extended family, but it's at least
	       an pentium4 core.  */
	    cpu = "pentium4";
	}
      if (has_sse3)
        {
	  if (has_longmode)
	    cpu = "nocona";
	  else 
            cpu = "prescott";
	}
    }

done:
  return concat ("-m", argv[0], "=", cpu, NULL);
}
#else
/* If we aren't compiling with GCC we just provide a minimal
   default value.  */
const char *host_detect_local_cpu (int argc, const char **argv)
{
  return concat ("-m", argv[0], "=i386", NULL);
}
#endif
#endif
