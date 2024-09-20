/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N (VECTOR_BITS * 6 / 16)

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

int main (int argc, const char* argv[])
{
  unsigned short input[N], output[N], i;
  unsigned short check_results[N];

  check_vect ();

  for (i = 0; i < N; i++)
    {
      input[i] = i;
      output[i] = 0;
      asm volatile ("" ::: "memory");
    }

  for (i = 0; i < N / 3; i++)
    {
      check_results[3*i] = 9 * i + 6;
      check_results[3*i+1] = 9 * i + 15;
      check_results[3*i+2] = 9 * i + 4;
      __asm__ volatile ("" : : : "memory");
    }

  foo (input, output);

#pragma GCC novector
  for (i = 0; i < N - (N % 3); i++)
     if (output[i] != check_results[i])
       abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 2 "vect" { target { ! { { vect_perm_short || vect32 } || vect_load_lanes } } } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { { vect_perm_short || vect32 } || vect_load_lanes } } } } */
/* We don't try permutes with a group size of 3 for variable-length
   vectors.  */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect" { target { ! { vect_perm_short || { vect32 || vect_load_lanes } } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target { vect_perm_short || { vect32 || vect_load_lanes } } } } } */
