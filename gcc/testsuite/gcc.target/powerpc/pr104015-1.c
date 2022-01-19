/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -ftree-vectorize -fno-vect-cost-model -fdump-tree-vect-details" } */

/* As PR104015, we don't expect vectorizer will re-try some vector modes
   for epilogues on Power9, since Power9 doesn't support partial vector
   by defaut.  */

#include <stdarg.h>
#define N 200

void __attribute__((noinline))
foo (unsigned short *__restrict__ pInput, unsigned short *__restrict__ pOutput)
{
  unsigned short i, a, b, c;

  for (i = 0; i < N / 3; i++)
    {
       a = *pInput++;
       b = *pInput++;
       c = *pInput++;

       *pOutput++ = a + b + c + 3;
       *pOutput++ = a + b + c + 12;
       *pOutput++ = a + b + c + 1;
    }
}

/* { dg-final { scan-tree-dump-not "Re-trying epilogue analysis with vector mode" "vect" } } */
