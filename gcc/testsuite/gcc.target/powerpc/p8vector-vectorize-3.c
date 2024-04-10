/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2 -ftree-vectorize -fvect-cost-model=dynamic" } */

#include <stddef.h>

#ifndef SIZE
#define SIZE 1024
#endif

#ifndef ALIGN
#define ALIGN 32
#endif

#define ALIGN_ATTR __attribute__((__aligned__(ALIGN)))

long long sign_ll[SIZE]	ALIGN_ATTR;
int	  sign_i [SIZE]	ALIGN_ATTR;

void copy_long_long_to_int (void)
{
  size_t i;

  for (i = 0; i < SIZE; i++)
    sign_i[i] = sign_ll[i];
}

/* { dg-final { scan-assembler "vpkudum" } } */
