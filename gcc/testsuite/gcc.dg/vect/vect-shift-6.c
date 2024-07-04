/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include <stdint.h>
#include "tree-vect.h"

#define N 32

int32_t A[N]; 
int32_t B[N];

#define FN(name)                   \
__attribute__((noipa))             \
void name(int32_t *a)                  \
{                                  \
  for (int i = 0; i < N / 2; i++)  \
    {                              \
       a[2 * i + 0] <<= i;         \
       a[2 * i + 1] <<= i;         \
    }                              \
}


FN(foo_vec)

#pragma GCC push_options
#pragma GCC optimize ("O0")
FN(foo_novec)
#pragma GCC pop_options

int main ()
{
  int i;

  check_vect ();

#pragma GCC novector
  for (i = 0; i < N; i++)
    A[i] = B[i] = -(i + 1);

  foo_vec(A);
  foo_novec(B);

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (A[i] != B[i])
      abort ();

  return 0;
}
