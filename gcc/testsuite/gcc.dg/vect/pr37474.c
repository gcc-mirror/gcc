/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>

#define M00 100
#define M10 216
#define M01 1322
#define M11 13
#define M02 74
#define M12 191

#define N 16

void foo (unsigned int *__restrict__ pInput, unsigned int *__restrict__ pOutput)
{
  unsigned int i, a, b, c, d, e, f;

  for (i = 0; i < N / 3; i++)
    {
       a = *pInput++;
       b = *pInput++;
       c = *pInput++;
       d = *pInput++;
       e = *pInput++;
       f = *pInput++;

       a = a + d;
       b = b + e;
       c = c + f;

       *pOutput++ = M00 * a + M01 * b + M02 * c;
       *pOutput++ = M10 * a + M11 * b + M12 * c;
    }
}

/* { dg-final { cleanup-tree-dump "vect" } } */

