/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -O2 -ftree-vectorize -fno-unroll-loops" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Verify that we can optimize a vector conditional move, where one of the arms
   is all 1's into using the mask as one of the inputs to XXSEL.  */

#include <altivec.h>

static int a[1024], b[1024], c[1024];

int *p_a = a, *p_b = b, *p_c = c;

void
test (void)
{
  unsigned long i;

  for (i = 0; i < 1024; i++)
    a[i] = (b[i] == c[i]) ? -1 : a[i];
}

/* { dg-final { scan-assembler-times {\mvcmpequw\M} 1 } } */
/* { dg-final { scan-assembler-times {\m(?:xxsel|xxlor|vor)\M} 1 } } */
/* { dg-final { scan-assembler-not {\mvspltisw\M} } } */
/* { dg-final { scan-assembler-not {\mxxlorc\M}   } } */
