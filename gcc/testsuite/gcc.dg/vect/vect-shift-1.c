/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 8

int main ()
{
  unsigned int A[N] = { 0x08000000, 0x08000001, 0x0ff0000ff, 0xf0000001,
			0x08000000, 0x08000001, 0x0ff0000ff, 0xf0000001 };
  unsigned int B[N] = { 0x01000000, 0x01000000, 0x01fe0001f, 0x1e000000,
			0x01000000, 0x01000000, 0x01fe0001f, 0x1e000000 };
  int i;

  check_vect ();

  for (i = 0; i < N; i++)
    A[i] = A[i] >> 3;

  /* check results:  */
  for (i = 0; i < N; i++)
    if (A[i] != B[i])
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
