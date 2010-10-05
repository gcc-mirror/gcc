/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include <stdio.h>
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

#define N 16

void foo (unsigned int *__restrict__ pInput,
          unsigned int *__restrict__ pOutput,
          unsigned int *__restrict__ pInput2,
          unsigned int *__restrict__ pOutput2)
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


       a = *pInput2++;
       b = *pInput2++;
       c = *pInput2++;
       d = *pInput2++;
       e = *pInput2++;

       *pOutput2++ = M00 * a + M01 * b + M02 * c + M03 * d + M04 * e;
       *pOutput2++ = M10 * a + M11 * b + M12 * c + M13 * d + M14 * e;
       *pOutput2++ = M20 * a + M21 * b + M22 * c + M23 * d + M24 * e;
       *pOutput2++ = M30 * a + M31 * b + M32 * c + M33 * d + M34 * e;
       *pOutput2++ = M40 * a + M41 * b + M42 * c + M43 * d + M44 * e;

    }
}

int main (int argc, const char* argv[])
{
  unsigned int input[N], output[N], i, input2[N], output2[N];
  unsigned int check_results[N] = {3208, 1334, 28764, 35679, 2789, 13028,
    4754, 168364, 91254, 12399, 22848, 8174, 307964, 146829, 22009, 0};
  unsigned int check_results2[N] = {7136, 2702, 84604, 57909, 6633, 16956,
    6122, 224204, 113484, 16243, 26776, 9542, 363804, 169059, 25853, 0};

  check_vect ();

  for (i = 0; i < N; i++)
    {
      input[i] = i%256;
      input2[i] = i + 2;
      output[i] = 0;
      output2[i] = 0;
      __asm__ volatile ("");
    }

  foo (input, output, input2, output2);

  for (i = 0; i < N; i++)
    if (output[i] != check_results[i]
        || output2[i] != check_results2[i])
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 0 "vect"  } } */
/* { dg-final { scan-tree-dump-times "permutation requires at least three vectors" 2 "vect" { target vect_perm } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect"  } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

