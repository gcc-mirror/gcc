/* { dg-additional-options "-fdump-tree-gimple" } */

void
f (int i, int j)
{
#pragma acc kernels
#pragma acc loop collapse(2)
  for (i = 0; i < 20; ++i)
    for (j = 0; j < 25; ++j)
      ;
}

/* { dg-final { scan-tree-dump-times "#pragma acc loop collapse\\(2\\) private\\(j\\) private\\(i\\)" 1 "gimple" } } */
