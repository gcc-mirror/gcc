/* Check that GCC does not generates Armv8.1-M low over head loop
   instructions.  Innermost loop has no fixed number of iterations
   therefore is not optimizable.  Outer loops are not optimized.  */
/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main+fp -mthumb -O3 --save-temps" } */
#include <stdlib.h>
#include "lob.h"

int a[N];
int b[N];
int c[N];

int
main (void)
{
  for (int i = 0; i < N; i++)
    {
      a[i] = i;
      b[i] = i * 2;

      int k = b[i];
      while (k != 0)
	{
	  if (k % 2 == 0)
	    c[i - 1] = k % 2;
	  k /= 2;
	}
      c[i] = a[i] - b[i];
    }

  return 0;
}
/* { dg-final { scan-assembler-not {dls\s\S*,\s\S*} } } */
/* { dg-final { scan-assembler-not {le\slr,\s\S*} } } */
