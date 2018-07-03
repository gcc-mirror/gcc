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

#define K00 405
#define K10 112
#define K01 4322
#define K11 135

#if VECTOR_BITS > 128
#define N (VECTOR_BITS * 3 / 32 + 4)
#else
#define N 16
#endif

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

       /* Regular SLP - no permutation required.  */
       *pOutput2++ = K00 * d;
       *pOutput2++ = K10 * e;
    }
}

int main (int argc, const char* argv[])
{
  int input[N], output[N], i;
  int input2[N], output2[N];

  check_vect ();

  for (i = 0; i < N; i++)
    {
      input[i] = i%256;
      input2[i] = i%256;
      output[i] = 0;
      output2[i] = 0;
      __asm__ volatile ("");
    }

#if N == 16
  int check_results[N] = { 1470, 395, 28271, 5958, 1655, 111653, 10446, 2915,
			   195035, 14934, 4175, 278417, 19422, 5435, 361799,
			   0 };
  int check_results2[N] = { 0, 112, 810, 336, 1620, 560, 2430, 784, 3240, 1008,
			    0, 0, 0, 0, 0, 0 };
#else
  volatile int check_results[N] = {};
  volatile int check_results2[N] = {};

  for (int i = 0; i < N / 3; i++)
    {
      int a = input[i * 3];
      int b = input[i * 3 + 1];
      int c = input[i * 3 + 2];
      int d = input2[i * 2];
      int e = input2[i * 2 + 1];

      check_results[i * 3] = M00 * a + M01 * b + M02 * c;
      check_results[i * 3 + 1] = M10 * a + M11 * b + M12 * c;
      check_results[i * 3 + 2] = M20 * a + M21 * b + M22 * c;

      check_results2[i * 2] = K00 * d;
      check_results2[i * 2 + 1] = K10 * e;

      asm volatile ("" ::: "memory");
    }
#endif

  foo (input, output, input2, output2);

  for (i = 0; i < N; i++)
     if (output[i] != check_results[i] || output2[i] != check_results2[i])
       abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect"  { target vect_perm } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target { vect_perm3_int && { ! vect_load_lanes } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target vect_load_lanes } } } */
/* { dg-final { scan-tree-dump "Built SLP cancelled: can use load/store-lanes" "vect" { target { vect_perm3_int && vect_load_lanes } } } } */
/* { dg-final { scan-tree-dump "LOAD_LANES" "vect" { target vect_load_lanes } } } */
/* { dg-final { scan-tree-dump "STORE_LANES" "vect" { target vect_load_lanes } } } */
