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

#define K00 405
#define K10 112
#define K01 4322
#define K11 135

#define N 16

void foo (int *__restrict__ pInput, int *__restrict__ pOutput,
          int *__restrict__ pInput2, int *__restrict__ pOutput2)
{
  int i, a, b, c, d, e;

  for (i = 0; i < N / 3; i++)
    {
       a = *pInput++;
       b = *pInput++;
       c = *pInput++;

       d = *pInput2++;
       e = *pInput2++;

       *pOutput++ = M00 * a + M01 * b + M02 * c;
       *pOutput++ = M10 * a + M11 * b + M12 * c;
       *pOutput++ = M20 * a + M21 * b + M22 * c;

       *pOutput2++ = K00 * d + K01 * e;
       *pOutput2++ = K10 * d + K11 * e;
    }
}

int main (int argc, const char* argv[])
{
  int input[N], output[N], i;
  int check_results[N] = {1470, 395, 28271, 5958, 1655, 111653, 10446, 2915, 195035, 14934, 4175, 278417, 19422, 5435, 361799, 0};
  int input2[N], output2[N];
  int check_results2[N] = {4322, 135, 13776, 629, 23230, 1123, 32684, 1617, 42138, 2111, 0, 0, 0, 0, 0, 0};

  check_vect ();

  check_vect ();

  for (i = 0; i < N; i++)
    {
      input[i] = i%256;
      input2[i] = i%256;
      output[i] = 0;
      output2[i] = 0;
      if (input[i] > 256)
        abort ();
    }

  foo (input, output, input2, output2);

  for (i = 0; i < N; i++)
     if (output[i] != check_results[i] || output2[i] != check_results2[i])
       abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_perm } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target vect_perm } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */


