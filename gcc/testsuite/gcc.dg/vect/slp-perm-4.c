/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_perm } */

#include <stdarg.h>
#include "tree-vect.h"

#define M00 100
#define M10 216
#define M20 23
#define M30 237
#define M40 437

#define M01 1322
#define M11 13
#define M21 27271
#define M31 2280
#define M41 284

#define M02 74
#define M12 191
#define M22 500
#define M32 111
#define M42 1114

#define M03 134
#define M13 117
#define M23 11
#define M33 771
#define M43 71

#define M04 334
#define M14 147
#define M24 115
#define M34 7716
#define M44 16

#if VECTOR_BITS > 128
#define N (VECTOR_BITS * 5 / 32)
#else
#define N 20
#endif

void foo (unsigned int *__restrict__ pInput, unsigned int *__restrict__ pOutput)
{
  unsigned int i, a, b, c, d, e;

  for (i = 0; i < N / 5; i++)
    {
       a = *pInput++;
       b = *pInput++;
       c = *pInput++;
       d = *pInput++;
       e = *pInput++;

       *pOutput++ = M00 * a + M01 * b + M02 * c + M03 * d + M04 * e;
       *pOutput++ = M10 * a + M11 * b + M12 * c + M13 * d + M14 * e;
       *pOutput++ = M20 * a + M21 * b + M22 * c + M23 * d + M24 * e;
       *pOutput++ = M30 * a + M31 * b + M32 * c + M33 * d + M34 * e;
       *pOutput++ = M40 * a + M41 * b + M42 * c + M43 * d + M44 * e;
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
      asm volatile ("" ::: "memory");
    }

#if N == 20
  unsigned int check_results[N]
    = {3208, 1334, 28764, 35679, 2789, 13028, 4754, 168364, 91254, 12399,
    22848, 8174, 307964, 146829, 22009, 32668, 11594, 447564, 202404, 31619};
#else
  volatile unsigned int check_results[N];

  for (i = 0; i < N / 5; i++)
    {
      unsigned int a = input[i * 5];
      unsigned int b = input[i * 5 + 1];
      unsigned int c = input[i * 5 + 2];
      unsigned int d = input[i * 5 + 3];
      unsigned int e = input[i * 5 + 4];

      check_results[i * 5] = M00 * a + M01 * b + M02 * c + M03 * d + M04 * e;
      check_results[i * 5 + 1] = (M10 * a + M11 * b + M12 * c
				  + M13 * d + M14 * e);
      check_results[i * 5 + 2] = (M20 * a + M21 * b + M22 * c
				  + M23 * d + M24 * e);
      check_results[i * 5 + 3] = (M30 * a + M31 * b + M32 * c
				  + M33 * d + M34 * e);
      check_results[i * 5 + 4] = (M40 * a + M41 * b + M42 * c
				  + M43 * d + M44 * e);
      asm volatile ("");
    }
#endif

  foo (input, output);

#pragma GCC novector
  for (i = 0; i < N; i++)
    if (output[i] != check_results[i])
      abort ();

  return 0;
}

/* Currently interleaving is not supported for a group-size of 5.  */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "gaps requires scalar epilogue loop" 0 "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" } } */
