/* PR tree-optimization/59643 */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-pcom-details" } */
/* { dg-additional-options "--param max-unrolled-insns=400" { target { powerpc*-*-* } } } */
/* Implicit threashold of max-unrolled-insn on ppc at O3 is too small for the
   loop of this case.  */

void
foo (double *a, double *b, double *c, double d, double e, int n)
{
  int i;
  for (i = 1; i < n - 1; i++)
    a[i] = d * (b[i] + c[i] + a[i - 1] + a[i + 1]) + e * a[i];
}

/* { dg-final { scan-tree-dump-times "Before commoning:" 1 "pcom" } } */
/* { dg-final { scan-tree-dump-times "Unrolling 2 times" 1 "pcom" } } */
