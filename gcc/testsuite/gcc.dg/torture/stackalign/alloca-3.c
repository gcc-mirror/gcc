/* PR middle-end/37009 */
/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-msse2" } */
/* { dg-require-effective-target sse2_runtime } */

#include <emmintrin.h>
#include "check.h"

#ifndef ALIGNMENT
#define ALIGNMENT	16
#endif

typedef int aligned __attribute__((aligned(ALIGNMENT)));

void
bar (char *p, int size)
{
  __builtin_strncpy (p, "good", size);
}

void
__attribute__ ((noinline))
foo (__m128 x, __m128 y ,__m128 z ,__m128 a, int size)
{
  char *p = __builtin_alloca (size + 1);
  aligned i;

  bar (p, size);
  if (__builtin_strncmp (p, "good", size) != 0)
    {
#ifdef DEBUG
      p[size] = '\0';
      printf ("Failed: %s != good\n", p);
#endif
      abort ();
    }

  if (check_int (&i,  __alignof__(i)) != i)
    abort ();
}

int
main (void)
{
  __m128 x = { 1.0 };
  foo (x, x, x, x, 5);

  return 0;
}
