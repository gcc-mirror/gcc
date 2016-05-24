/* { dg-additional-options "-fdump-tree-original" } */

void
f (char *cp)
{
#pragma acc parallel pcopyout(cp[5:7])
  ;
}

/* { dg-final { scan-tree-dump-times "#pragma acc parallel map\\(from:\\*\\(cp \\+ 5\\) \\\[len: 7]\\) map\\(firstprivate:cp \\\[pointer assign, bias: 5]\\)" 1 "original" } } */
