/* { dg-do compile } */
/* { dg-options "-O -w -fdump-tree-fre1-vops" } */

double foo (double) __attribute__ ((pure));
double (*fp) (double) __attribute__ ((const));
double f(double a)
{
  fp = foo;
  /* Verify when propagating foo to the call we preserve its constness.  */
  return fp (a);
}

/* { dg-final { scan-tree-dump "foo \\(a" "fre1" } } */
/* { dg-final { scan-tree-dump-times "VUSE" 1 "fre1" } } */
