/* Check that GCC does not generate Armv8.1-M low over head loop instructions
   if a non-inlineable function call takes place inside the loop.  */
/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main -O3 --save-temps" } */
#include <stdlib.h>
#include "lob.h"

int a[N];
int b[N];
int c[N];

int __attribute__ ((noinline))
foo (int a, int b)
{
  return a + b;
}

int
main (void)
{
  for (int i = 0; i < N; i++)
    {
      a[i] = i;
      b[i] = i * 2;
      c[i] = foo (a[i], b[i]);
    }

  return 0;
}
/* { dg-final { scan-assembler-not {dls\s\S*,\s\S*} } } */
/* { dg-final { scan-assembler-not {le\slr,\s\S*} } } */
