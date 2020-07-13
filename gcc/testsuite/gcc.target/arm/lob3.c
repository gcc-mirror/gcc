/* Check that GCC does not generate Armv8.1-M low over head loop instructions
   if causes VFP emulation library calls to happen inside the loop.  */
/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main -O3 --save-temps -mfloat-abi=soft" } */
/* { dg-require-effective-target arm_softfloat } */
#include <stdlib.h>
#include "lob.h"

double a[N];
double b[N];
double c[N];

int
main (void)
{
  for (int i = 0; i < N; i++)
    {
      a[i] = i;
      b[i] = i * 2;
      c[i] = a[i] + b[i];
    }

  return 0;
}
/* { dg-final { scan-assembler-not {dls\s\S*,\s\S*} } } */
/* { dg-final { scan-assembler-not {le\slr,\s\S*} } } */
