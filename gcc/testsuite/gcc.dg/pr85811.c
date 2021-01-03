/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
#include <stdio.h>

int main() {
  const double negval = -1.0;
  const double nanval = 0.0 / 0.0;
  const double val = __builtin_fmax(negval, nanval);
  const double absval = __builtin_fabs(val);
  printf("fabs(%.16e) = %.16e\n", val, absval);
  return absval >= 0 ? 0 : 1;
}

/* We hope not to see:  printf ("fabs(%.16e) = %.16e\n", val_4, val_4); */
/* { dg-final { scan-tree-dump-not "val_\[0-9\]*, val_\[0-9\]*" "optimized" } } */
