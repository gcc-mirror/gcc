/* PR target/125932 */
/* { dg-do run } */
/* { dg-require-effective-target sse2 } */
/* { dg-options "-O2 -msse2" } */

#include "sse2-check.h"

typedef int v4si __attribute__ ((vector_size (16)));

int a;

__attribute__((noipa)) void
f (v4si c, int v, int t)
{
  int x;

  asm goto ("movl %1, %0\n\t"
	    "testl %2, %2\n\t"
	    "jnz %l[d]"
	    : "=&r" (x) : "r" (v), "r" (t) : : d);
  a = x & c[1] & c[2] & c[3];
  return;

 d:
  a = x & c[0] & c[2] & c[3];
}

static void
sse2_test (void)
{
  v4si m = { -1, -1, -1, -1 };

  f (m, 0x1234, 1);
  if (a != 0x1234)
    __builtin_abort ();

  f (m, 0x5678, 0);
  if (a != 0x5678)
    __builtin_abort ();
}
