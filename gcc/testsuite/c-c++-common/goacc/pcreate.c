/* { dg-additional-options "-fdump-tree-original" } */

void
f (char *cp)
{
#pragma acc parallel pcreate(cp[6:8])
  ;
}

/* { dg-final { scan-tree-dump-times "#pragma acc parallel map\\(alloc:\\*\\(cp \\+ 6\\) \\\[len: 8]\\) map\\(firstprivate:cp \\\[pointer assign, bias: 6]\\)" 1 "original" } } */
