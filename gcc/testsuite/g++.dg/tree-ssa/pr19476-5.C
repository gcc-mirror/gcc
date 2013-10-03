/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

// See pr19476-1.C for a version that includes <new>.

int g(){
  return 42 + (0 == new int[50]);
}

/* { dg-final { scan-tree-dump     "return 42" "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
