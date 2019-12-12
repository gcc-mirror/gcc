/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-recip" } */
/* { dg-additional-options "-fcompare-debug" { target { ! powerpc-ibm-aix* } } } */

/* We want to do the recip_sqrt transformations here there is already
   a multiplication on the main path.  */

double res, res2, tmp;
void
foo (double a, double b, int c, int d)
{
  tmp = 1.0 / __builtin_sqrt (a);
  res = tmp * tmp;

  if (d)
    res2 = a * tmp;
}

/* { dg-final { scan-tree-dump "Optimizing reciprocal sqrt multiplications" "recip" } } */
/* { dg-final { scan-tree-dump "Replacing squaring multiplication" "recip" } } */
/* { dg-final { scan-tree-dump "Replacing original division" "recip" } } */
