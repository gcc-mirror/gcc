/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#if VECTOR_BITS > 512
#define N (VECTOR_BITS * 6 / 16)
#else
#define N 200
#endif

void __attribute__((noinline))
foo (unsigned char *__restrict__ pInput, unsigned char *__restrict__ pOutput)
{
  unsigned char a, b, c;
  unsigned int i;

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
  unsigned char input[N], output[N];
  unsigned char check_results[N];
  unsigned int i;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      input[i] = i;
      output[i] = 0;
      __asm__ volatile ("");
    }

  for (i = 0; i < N / 3; i++)
    {
      check_results[3*i] = 9 * i + 6;
      check_results[3*i+1] = 9 * i + 15;
      check_results[3*i+2] = 9 * i + 4;
      __asm__ volatile ("" : : : "memory");
    }

  foo (input, output);

  for (i = 0; i < N - (N % 3); i++)
     if (output[i] != check_results[i])
       abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_perm_byte } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "vect" { target { vect_perm3_byte && { { ! vect_load_lanes } && { { ! vect_partial_vectors_usage_1 } || s390_vx } } } } } } */
/* The epilogues are vectorized using partial vectors.  */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 2 "vect" { target { vect_perm3_byte && { { ! vect_load_lanes } && { vect_partial_vectors_usage_1 && { ! s390_vx } } } } } } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect" { target vect_load_lanes } } } */
/* { dg-final { scan-tree-dump "Built SLP cancelled: can use load/store-lanes" "vect" { target { vect_perm3_byte && vect_load_lanes } } } } */
/* { dg-final { scan-tree-dump "LOAD_LANES" "vect" { target vect_load_lanes } } } */
/* { dg-final { scan-tree-dump "STORE_LANES" "vect" { target vect_load_lanes } } } */
