/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdelete-null-pointer-checks -fno-allocation-dce" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

#include <new>

int f(){
  int *p = new(std::nothrow) int;
  return 33 + (0 == p);
}
int g(){
  int *p = new int[50];
  return 42 + (0 == p);
}

/* { dg-final { scan-tree-dump     "return 42" "optimized" } } */
/* { dg-final { scan-tree-dump-not "return 33" "optimized" } } */
