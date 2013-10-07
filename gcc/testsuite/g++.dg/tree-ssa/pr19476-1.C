/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

// See pr19476-5.C for a version without including <new>.
#include <new>

int f(){
  return 33 + (0 == new(std::nothrow) int);
}
int g(){
  return 42 + (0 == new int[50]);
}

/* { dg-final { scan-tree-dump     "return 42" "ccp1" } } */
/* { dg-final { scan-tree-dump-not "return 33" "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
