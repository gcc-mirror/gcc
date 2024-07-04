/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "--param max-completely-peel-times=6" } */

#include <stdarg.h>
#include <stdint.h>
#include "tree-vect.h"

#define N 16
#define M 16

int32_t A[N];
int32_t B[N];

#define FN(name)                   \
__attribute__((noipa))             \
void name(int32_t *a, int m)       \
{                                  \
  for (int i = 0; i < N / 2; i++)  \
    {                              \
      int s1 = i;                  \
      int s2 = s1 + 1;             \
      int32_t r1 = 0;              \
      int32_t r2 = 7;              \
      int32_t t1 = m;              \
		                   \
      for (int j = 0; j < M; j++)  \
         {                         \
            r1 += t1 << s1;        \
            r2 += t1 << s2;        \
            t1++;                  \
            s1++;                  \
            s2++;                  \
         }                         \
                                   \
       a[2 * i + 0] = r1;          \
       a[2 * i + 1] = r2;          \
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
    A[i] = B[i] = 0;

  foo_vec(A, 0);
  foo_novec(B, 0);

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    if (A[i] != B[i])
      abort ();

  return 0;
}
