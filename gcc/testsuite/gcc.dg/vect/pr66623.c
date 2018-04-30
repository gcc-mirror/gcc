/* { dg-require-effective-target vect_float } */

#include "tree-vect.h"

extern void abort (void);

#define OP *
#define INIT 1.0

float __attribute__((noinline,noclone))
foo (float *__restrict__ i)
{
  float l = INIT;
  int a;
  int b;

  for (a = 0; a < 4; a++)
    for (b = 0; b < 4; b++)
      l = l OP i[b];

  return l;
}

float __attribute__((noinline,noclone))
foo_ref (float *__restrict__ i)
{
  float l = INIT;

  l = l OP i[0];
  l = l OP i[1];
  l = l OP i[2];
  l = l OP i[3];

  l = l OP i[0];
  l = l OP i[1];
  l = l OP i[2];
  l = l OP i[3];

  l = l OP i[0];
  l = l OP i[1];
  l = l OP i[2];
  l = l OP i[3];

  l = l OP i[0];
  l = l OP i[1];
  l = l OP i[2];
  l = l OP i[3];

  return l;
}

union u
{
  float f;
  unsigned int u;
};

int
main (void)
{
  union u res, res2;
  float a[4];

  if (sizeof (float) != sizeof (unsigned int))
    return 0;

  check_vect ();

  a[0] = 0.01;
  a[1] = 0.01;
  a[2] = 0.01;
  a[3] = 1.0;

  res.f = foo_ref (a);

  res2.f = foo (a);

  if (res.u != res2.u)
    abort ();

  return 0;
}

/* need -ffast-math to vectorize this loop.  */
/* ARM NEON passes -ffast-math to these tests, so expect this to fail.  */
/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" { xfail arm_neon_ok } } } */
