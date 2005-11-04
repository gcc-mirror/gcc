/* PR 17930 */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O1 -msse2 -mfpmath=sse -mno-accumulate-outgoing-args -fno-omit-frame-pointer" } */

#include "../../gcc.dg/i386-cpuid.h"

typedef _Complex double complex_16;

void NOINLINE
test (complex_16 a[5][5])
{
  int i, j, k;
  complex_16 x;

  for (j = 0; j < 5; j++)
    for (i = 0; i < 5; i++)
      {
        for (k = 0; k < j - 1; ++k)
	  x = a[k][i] * ~a[k][j];
	a[j][i] = x;
      }
}

int main()
{
  static complex_16 work[5][5];
  unsigned long cpu_facilities;

  cpu_facilities = i386_cpuid ();
  if (cpu_facilities & bit_SSE2)
    test (work); 

  return 0;
}
