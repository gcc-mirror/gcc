/* Check that GCC does not generate Armv8.1-M low over head loop instructions
   if LR is modified within the loop.  */
/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main -O3 --save-temps -mfloat-abi=soft" } */
/* { dg-require-effective-target arm_softfloat } */
#include <stdlib.h>
#include "lob.h"

int a[N];
int b[N];
int c[N];

static __attribute__ ((always_inline)) inline int
foo (int a, int b)
{
  NO_LOB;
  return a + b;
}

int
main (void)
{
  for (int i = 0; i < N; i++)
    {
      a[i] = i;
      b[i] = i * 2;
      c[i] = foo(a[i], b[i]);
    }

  return 0;
}
/* { dg-final { scan-assembler-not {dls\s\S*,\s\S*} } } */
/* { dg-final { scan-assembler-not {le\slr,\s\S*} } } */
