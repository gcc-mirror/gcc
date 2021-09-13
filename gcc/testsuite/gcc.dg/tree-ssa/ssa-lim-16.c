/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim2-details" } */

volatile int flag, bar;
double foo (double *valp)
{
  double sum = 0;
  for (int i = 0; i < 256; ++i)
    {
      for (int j = 0; j < 256; ++j)
        bar = flag;
      if (flag)
        sum += 1.;
      sum += *valp; // we should move the load of *valp out of the loop
    }
  return sum;
}

/* { dg-final { scan-tree-dump-times "Moving statement" 1 "lim2" } } */
