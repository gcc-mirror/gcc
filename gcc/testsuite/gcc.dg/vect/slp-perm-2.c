/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define M00 100
#define M10 216
#define M01 1322
#define M11 13
#define M02 74
#define M12 191

#define N 16

void foo (unsigned int *__restrict__ pInput, unsigned int *__restrict__ pOutput)
{
  unsigned int i, a, b;

  for (i = 0; i < N / 2; i++)
    {
       a = *pInput++;
       b = *pInput++;

       *pOutput++ = M00 * a + M01 * b;
       *pOutput++ = M10 * a + M11 * b;
    }
}

int main (int argc, const char* argv[])
{
  unsigned int input[N], output[N], i;
  unsigned int check_results[N] = {1322, 13, 4166, 471, 7010, 929, 9854, 1387, 12698, 1845, 15542, 2303, 18386, 2761, 21230, 3219};

  check_vect ();

  for (i = 0; i < N; i++)
    {
      input[i] = i%256;
      output[i] = 0;
      __asm__ volatile ("");
    }

  foo (input, output);

  for (i = 0; i < N; i++)
    {
      if (output[i] != check_results[i])
	abort ();
      __asm__ volatile ("");
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_perm } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target vect_perm } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

