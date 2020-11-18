/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */
/* { dg-do run { target { s390_z14_hw } } } */
#include <assert.h>

typedef float tf __attribute__ ((mode (TF)));
static tf x;
static tf y;

__attribute__ ((noipa)) static tf
sub (void)
{
  return x - y;
}

int
main (void)
{
  x = 1.5L;
  y = 2.5L;
  assert (sub () == -1.0L);
}
