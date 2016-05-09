/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-ifcvt-stats" } */
/* { dg-require-visibility "" } */

int a[1024] = {0.0};
int b[1024] = {0.0};
int c[1024] = {0.0};
int foo (float *x)
{
  int i = 0;

  for (i = 0; i < 1024; i++)
    {
      c[i] = (x[i] > 0.0) ? a[i] : b[i];
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */
