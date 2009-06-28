/* { dg-do run } */
/* { dg-options "-msse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

#ifdef __i386__
#include "cpuid.h"
#endif

typedef float v2sf __attribute__ ((vector_size (2 * sizeof(float))));

static void test (void)
{
  v2sf a = {1.0, 0.0};
  v2sf b = {0.0, 1.0};
  v2sf d;
  d = a + b;
}

int main ()
{

#ifdef __i386__
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  if (!(edx & bit_SSE))
    return 0;
#endif

  test ();

  return 0;
}
