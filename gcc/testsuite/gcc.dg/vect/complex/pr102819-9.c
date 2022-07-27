/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-add-options arm_v8_3a_complex_neon } */

#include <stdio.h>
#include <complex.h>

#define N 200
#define TYPE float
#define TYPE2 float

void g (TYPE2 complex a[restrict N], TYPE complex b[restrict N], TYPE complex c[restrict N])
{
  for (int i=0; i < N; i++)
    {
      c[i] -=  a[i] * b[0];
    }
}

/* The pattern overlaps with COMPLEX_ADD so we need to support consuming ADDs in COMPLEX_FMS.  */

/* { dg-final { scan-tree-dump "Found COMPLEX_FMS" "vect" { xfail { vect_float } } } } */
