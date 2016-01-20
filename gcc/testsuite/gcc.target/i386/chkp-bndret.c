/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-chkp" } */
/* { dg-final { scan-tree-dump-not "bndret" "chkp" } } */

#include "string.h"

extern int *test1 (int *p) __attribute__((bnd_legacy));

int *
test2 (int *p)
{
  return test1 (p);
}
