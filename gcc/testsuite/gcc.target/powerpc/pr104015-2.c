/* { dg-require-effective-target power10_ok } */
/* Vector with length instructions lxvl/stxvl are only enabled for 64 bit.  */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model -fdump-tree-vect-details" } */

/* Power10 support partial vector for epilogue by default, it's expected
   vectorizer would re-try for it once.  */

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

/* { dg-final { scan-tree-dump-times "Re-trying epilogue analysis with vector mode" 1 "vect" } } */
