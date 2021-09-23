/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-vectorize -fdump-tree-pcom-details -fdisable-tree-vect" } */

extern double arr[100];
extern double foo (double, double);
extern double sum;

void
test (int i_0, int i_n)
{
  int i;
  for (i = i_0; i < i_n - 1; i++)
    {
      double a = arr[i];
      double b = arr[i + 1];
      sum += a * b;
    }
}

/* { dg-final { scan-tree-dump "Executing predictive commoning without unrolling" "pcom" } } */
