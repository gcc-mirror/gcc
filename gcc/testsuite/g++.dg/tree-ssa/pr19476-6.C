/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

// See pr19476-7.C for a version without including <new>.
#include <new>

int f(){
  return 33 + (0 == new(std::nothrow) int);
}
int g(){
  return 42 + (0 == new int[50]);
}

/* { dg-final { scan-tree-dump     "return 42" "evrp" } } */
/* { dg-final { scan-tree-dump-not "return 33" "evrp" } } */
