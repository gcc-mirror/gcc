/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include <stdio.h>
#include "tree-vect.h"

#define M00 100
#define M10 216
#define M20 23
#define M01 1322
#define M11 13
#define M21 27271
#define M02 74
#define M12 191
#define M22 500

#define N 16

void foo (unsigned int *__restrict__ pInput, unsigned int *__restrict__ pOutput)
{
  unsigned int i, a, b, c;

  for (i = 0; i < N / 3; i++)
    {
       a = *pInput++;
       b = *pInput++;
       c = *pInput++;

       *pOutput++ = M00 * a + M01 * b + M02 * c;
       *pOutput++ = M10 * a + M11 * b + M12 * c;
       *pOutput++ = M20 * a + M21 * b + M22 * c;
    }
}

int main (int argc, const char* argv[])
{
  unsigned int input[N], output[N], i;
  unsigned int check_results[N] = {1470, 395, 28271, 5958, 1655, 111653, 10446, 2915, 195035, 14934, 4175, 278417, 19422, 5435, 361799, 0};

  check_vect ();

  for (i = 0; i < N; i++)
    {
      input[i] = i%256;
      if (input[i] > 200)
        abort();
      output[i] = 0;
    }

  foo (input, output);

  for (i = 0; i < N; i++)
     if (output[i] != check_results[i])
       abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_perm } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target vect_perm } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

