/* PR middle-end/37010 */
/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-msse2 -mpreferred-stack-boundary=2" } */

#include <emmintrin.h>
#include "cpuid.h"

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));
typedef int aligned __attribute__((aligned(16)));

extern void abort (void);

__m128 r;

int
__attribute__ ((noinline))
check_int (int *i, int align)
{
  *i = 20;
  if ((((ptrdiff_t) i) & (align - 1)) != 0)
    {
      abort ();
    }
  return *i;
}

void
__attribute__ ((noinline))
foo (__m128 x, __m128 y ,__m128 z ,__m128 a, int size)
{
  aligned i;

  if (size != 5 || check_int (&i, __alignof__(i)) != i)
    abort ();

  r = a;
}

int
main (void)
{
  __m128 x = { 1.0 };
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE2 test only if host has SSE2 support.  */
  if (edx & bit_SSE2)
    {
      foo (x, x, x, x, 5);

      if (__builtin_memcmp (&r, &x, sizeof (r)))
	abort ();
    }

  return 0;
}
