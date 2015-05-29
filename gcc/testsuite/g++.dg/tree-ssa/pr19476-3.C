/* { dg-do compile } */
/* { dg-options "-O3 -fcheck-new -fdump-tree-optimized" } */

#include <new>

int g(){
  return 42 + (0 == new int);
}

/* { dg-final { scan-tree-dump-not "return 42" "optimized" } } */
