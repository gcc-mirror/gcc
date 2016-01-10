/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define M00 100
#define M10 216
#define M20 23
#define M30 237
#define M01 1322
#define M11 13
#define M21 27271
#define M31 2280
#define M02 74
#define M12 191
#define M22 500
#define M32 111
#define M03 134
#define M13 117
#define M23 11
#define M33 771

#define N 16

void foo (unsigned int *__restrict__ pInput, unsigned int *__restrict__ pOutput)
{
  unsigned int i, a, b, c, d;

  for (i = 0; i < N / 4; i++)
    {
       a = *pInput++;
       b = *pInput++;
       c = *pInput++;
       d = *pInput++;

       *pOutput++ = M00 * a + M01 * b + M02 * c + M03 * d;
       *pOutput++ = M10 * a + M11 * b + M12 * c + M13 * d;
       *pOutput++ = M20 * a + M21 * b + M22 * c + M23 * d;
       *pOutput++ = M30 * a + M31 * b + M32 * c + M33 * d;
    }
}

int main (int argc, const char* argv[])
{
  unsigned int input[N], output[N], i;
  unsigned int check_results[N] = {1872, 746, 28304, 4815, 8392, 2894, 139524, 18411, 14912, 5042, 250744, 32007, 21432, 7190, 361964, 45603};

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
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target { vect_perm && {! vect_load_lanes } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect" { target vect_load_lanes } } } */
/* { dg-final { scan-tree-dump "note: Built SLP cancelled: can use load/store-lanes" "vect" { target { vect_perm && vect_load_lanes } } } } */
/* { dg-final { scan-tree-dump "LOAD_LANES" "vect" { target vect_load_lanes } } } */
/* { dg-final { scan-tree-dump "STORE_LANES" "vect" { target vect_load_lanes } } } */

