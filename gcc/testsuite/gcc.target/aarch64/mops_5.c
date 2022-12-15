/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.6-a+mops" } */

#include <stddef.h>

void g();
void foo (int a, size_t N, char *__restrict__ in,
         char *__restrict__ out)
{
  if (a != 3)
    __builtin_memcpy (out, in, N);
  if (a > 3)
    g ();
}

/* { dg-final { scan-assembler-times {cmp\tw0, *} 2 } } */

