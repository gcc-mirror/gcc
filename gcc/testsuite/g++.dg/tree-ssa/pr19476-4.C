/* { dg-do compile } */
/* { dg-options "-O3 -fno-delete-null-pointer-checks -fdump-tree-optimized" } */

#include <new>

int g(){
  return 42 + (0 == new int);
}

/* { dg-final { scan-tree-dump-not "return 42" "optimized" } } */
