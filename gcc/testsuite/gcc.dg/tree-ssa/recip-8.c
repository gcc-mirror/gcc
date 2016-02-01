/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-optimized" } */

double bar (double, double, double, double, double);

double
foo (double a)
{
  return bar (1.0/a, 2.0/a, 4.0/a, 8.0/a, 16.0/a);
}

/* { dg-final { scan-tree-dump-times "/" 1 "optimized"  } } */
