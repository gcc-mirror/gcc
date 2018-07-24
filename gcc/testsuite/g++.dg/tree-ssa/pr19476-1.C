/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-dom2 -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

// See pr19476-5.C for a version without including <new>.
#include <new>

int f(){
  return 33 + (0 == new(std::nothrow) int);
}
int g(){
  return 42 + (0 == new int[50]);
}

/* { dg-final { scan-tree-dump     "return 42" "dom2" } } */
/* { dg-final { scan-tree-dump-not "return 33" "dom2" } } */
