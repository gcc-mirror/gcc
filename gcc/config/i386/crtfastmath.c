/*
 * Copyright (C) 2005, 2007, 2009 Free Software Foundation, Inc.
 *
 * This file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * 
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 *
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#define MXCSR_DAZ (1 << 6)	/* Enable denormals are zero mode */
#define MXCSR_FTZ (1 << 15)	/* Enable flush to zero mode */

#ifndef __x86_64__
/* All 64-bit targets have SSE and DAZ;
   only check them explicitly for 32-bit ones. */
#include "cpuid.h"
#endif

static void __attribute__((constructor))
#ifndef __x86_64__
/* The i386 ABI only requires 4-byte stack alignment, so this is necessary
   to make sure the fxsave struct gets correct alignment.
   See PR27537 and PR28621.  */
__attribute__ ((force_align_arg_pointer))
#endif
set_fast_math (void)
{
#ifndef __x86_64__
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return;

  if (edx & bit_SSE)
    {
      unsigned int mxcsr = __builtin_ia32_stmxcsr ();
  
      mxcsr |= MXCSR_FTZ;

      if (edx & bit_FXSAVE)
	{
	  /* Check if DAZ is available.  */
	  struct
	    {
	      unsigned short int cwd;
	      unsigned short int swd;
	      unsigned short int twd;
	      unsigned short int fop;
	      long int fip;
	      long int fcs;
	      long int foo;
	      long int fos;
	      long int mxcsr;
	      long int mxcsr_mask;
	      long int st_space[32];
	      long int xmm_space[32];
	      long int padding[56];
	    } __attribute__ ((aligned (16))) fxsave;

	  __builtin_memset (&fxsave, 0, sizeof (fxsave));

	  asm volatile ("fxsave %0" : "=m" (fxsave) : "m" (fxsave));

	  if (fxsave.mxcsr_mask & MXCSR_DAZ)
	    mxcsr |= MXCSR_DAZ;
	}

      __builtin_ia32_ldmxcsr (mxcsr);
    }
#else
  unsigned int mxcsr = __builtin_ia32_stmxcsr ();
  mxcsr |= MXCSR_DAZ | MXCSR_FTZ;
  __builtin_ia32_ldmxcsr (mxcsr);
#endif
}
