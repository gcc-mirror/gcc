/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-recip" } */
/* { dg-additional-options "-fcompare-debug" { target { ! powerpc-ibm-aix* } } } */

/* The main path doesn't have any multiplications.
   Avoid introducing them in the recip pass.  */

double res, res2, tmp;
void
foo (double a, double b, int c, int d)
{
  tmp = 1.0 / __builtin_sqrt (a);
  if (c)
    res = tmp * tmp;

  if (d)
    res2 = a * tmp;
}

/* { dg-final { scan-tree-dump-not "Optimizing reciprocal sqrt multiplications" "recip" } } */
/* { dg-final { scan-tree-dump-not "Replacing squaring multiplication" "recip" } } */
/* { dg-final { scan-tree-dump-not "Replacing original division" "recip" } } */
