/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-gimple" } */

void
foo (void)
{
  unsigned int i = 0;
#pragma acc kernels
  {
    i++;
  }
}

/* { dg-final { scan-tree-dump-times "map\\(force_tofrom" 1 "gimple" } } */
