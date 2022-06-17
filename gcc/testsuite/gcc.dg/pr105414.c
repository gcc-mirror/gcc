/* { dg-do run { target { *-*-linux* *-*-gnu* } } } */
/* { dg-options "-O1 -fsignaling-nans -lm" } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target issignaling } */


#define _GNU_SOURCE
#include <stdio.h>
#include <math.h>

int main()
{
  double a = __builtin_nans ("");

  if (issignaling (fmin (a, a)))
    __builtin_abort ();

  if (issignaling (fmax (a, a)))
    __builtin_abort ();

  double b = __builtin_nan ("");

  if (issignaling (fmin (a, b)))
    __builtin_abort ();

  if (issignaling (fmax (a, b)))
    __builtin_abort ();

  return 0;
}
