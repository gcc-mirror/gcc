/* { dg-do run } */
/* { dg-require-effective-target vect_float} */
/* { dg-additional-options "-w -Wno-psabi -ffast-math" } */

#include "tree-vect.h"

typedef float v4sf __attribute__((vector_size(sizeof(float)*4)));

float __attribute__((noipa))
f(v4sf v)
{
  return v[0]+v[1]+v[2]+v[3];
}

float __attribute__((noipa))
g(float *v)
{
  return v[0]+v[1]+v[2]+v[3];
}

float __attribute__((noipa))
h(float *v)
{
  return 2*v[0]+3*v[1]+4*v[2]+5*v[3];
}

int
main ()
{
  check_vect ();
  v4sf v = (v4sf) { 1.f, 3.f, 4.f, 2.f };
  if (f (v) != 10.f)
    abort ();
  if (g (&v[0]) != 10.f)
    abort ();
  if (h (&v[0]) != 37.f)
    abort ();
  return 0;
}

/* We are lacking an effective target for .REDUC_PLUS support.  */
/* { dg-final { scan-tree-dump-times "basic block part vectorized" 3 "slp2" { target x86_64-*-* } } } */
/* { dg-final { scan-tree-dump-not " = VEC_PERM_EXPR" "slp2" { target x86_64-*-* } } } */
