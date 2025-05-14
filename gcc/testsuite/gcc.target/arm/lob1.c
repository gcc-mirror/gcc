/* Check that GCC generates Armv8.1-M low over head loop instructions
   for some simple loops.  */
/* { dg-do run } */
/* { dg-require-effective-target arm_v8_1m_lob_hw } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main -mthumb -O3 --save-temps" } */
#include <stdlib.h>
#include "lob.h"

int a[N];
int b[N];
int c[N];

int
foo (int a, int b)
{
  return a + b;
}

void __attribute__((noinline))
loop1 (int *a, int *b, int *c)
{
  for (int i = 0; i < N; i++)
    {
      a[i] = i;
      b[i] = i * 2;
      c[i] = a[i] + b[i];
    }
}

void __attribute__((noinline))
loop2 (int *a, int *b, int *c)
{
  int i = 0;
  while (i < N)
    {
      a[i] = i - 2;
      b[i] = i * 5;
      c[i] = a[i] + b[i];
      i++;
    }
}

void __attribute__((noinline))
loop3 (int *a, int *b, int *c)
{
  int i = 0;
  do
    {
      a[i] = i - 4;
      b[i] = i * 3;
      c[i] = a[i] + b[i];
      i++;
    } while (i < N);
}

int
main (void)
{
  reset_data (a, b, c, N);
  loop1 (a, b ,c);
  check_plus (a, b, c, N);
  reset_data (a, b, c, N);
  loop2 (a, b ,c);
  check_plus (a, b, c, N);
  reset_data (a, b, c, N);
  loop3 (a, b ,c);
  check_plus (a, b, c, N);

  return 0;
}

/* { dg-final { scan-assembler-times {dls\s\S*,\s\S*} 3 } } */
/* { dg-final { scan-assembler-times {le\slr,\s\S*} 3 } } */
