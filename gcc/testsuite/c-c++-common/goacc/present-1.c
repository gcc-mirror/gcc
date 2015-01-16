/* { dg-additional-options "-fdump-tree-original" } */

void
f (char *cp)
{
#pragma acc parallel present(cp[7:9])
  ;
}

/* { dg-final { scan-tree-dump-times "#pragma acc parallel map\\(force_present:\\*\\(cp \\+ 7\\) \\\[len: 9]\\) map\\(alloc:cp \\\[pointer assign, bias: 7]\\)" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
