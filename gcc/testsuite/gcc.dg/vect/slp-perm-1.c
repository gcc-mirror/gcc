/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
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

#if VECTOR_BITS > 128
#define N (VECTOR_BITS * 3 / 32 + 4)
#else
#define N 16
#endif

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

  check_vect ();

  for (i = 0; i < N; i++)
    {
      input[i] = i%256;
      output[i] = 0;
      __asm__ volatile ("");
    }

#if N == 16
  unsigned int check_results[N] = {1470, 395, 28271, 5958, 1655, 111653, 10446, 2915, 195035, 14934, 4175, 278417, 19422, 5435, 361799, 0};
#else
  volatile unsigned int check_results[N] = {};

  for (unsigned int i = 0; i < N / 3; i++)
    {
      unsigned int a = input[i * 3];
      unsigned int b = input[i * 3 + 1];
      unsigned int c = input[i * 3 + 2];

      check_results[i * 3] = M00 * a + M01 * b + M02 * c;
      check_results[i * 3 + 1] = M10 * a + M11 * b + M12 * c;
      check_results[i * 3 + 2] = M20 * a + M21 * b + M22 * c;

      asm volatile ("" ::: "memory");
    }
#endif

  foo (input, output);

#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (output[i] != check_results[i])
	abort ();
      __asm__ volatile ("");
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_perm } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target { vect_perm3_int || vect_load_lanes } } } } */
/* { dg-final { scan-tree-dump "can use load/store-lanes" "vect" { target { vect_perm3_int && vect_load_lanes } } } } */
/* { dg-final { scan-tree-dump "LOAD_LANES" "vect" { target vect_load_lanes } } } */
/* { dg-final { scan-tree-dump "STORE_LANES" "vect" { target vect_load_lanes } } } */

