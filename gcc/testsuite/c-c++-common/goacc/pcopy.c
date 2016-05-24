/* { dg-additional-options "-fdump-tree-original" } */

void
f (char *cp)
{
#pragma acc parallel pcopy(cp[3:5])
  ;
}

/* { dg-final { scan-tree-dump-times "#pragma acc parallel map\\(tofrom:\\*\\(cp \\+ 3\\) \\\[len: 5]\\) map\\(firstprivate:cp \\\[pointer assign, bias: 3]\\)" 1 "original" } } */
