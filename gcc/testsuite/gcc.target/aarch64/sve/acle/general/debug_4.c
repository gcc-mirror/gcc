/* { dg-options "-O -g -msve-vector-bits=512" } */

#include <arm_sve.h>

void __attribute__((noipa))
g (volatile int *x, svbool_t pg)
{
  *x = 1;
}

void
f (volatile int *x)
{
  svbool_t pg = svorr_z (svpfalse (), svpfalse (), svpfalse ());
  g (x, pg);
}
