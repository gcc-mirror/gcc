/* PR middle-end/37009 */
/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-msse2" } */

#include <emmintrin.h>
#include "cpuid.h"
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
foo (__m128 x, __m128 y ,__m128 z , int size)
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
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE2 test only if host has SSE2 support.  */
  if (edx & bit_SSE2)
    foo (x, x, x, 5);

  return 0;
}
