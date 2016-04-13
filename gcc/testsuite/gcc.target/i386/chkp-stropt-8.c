/* { dg-do compile { target { ! x32 } } } */
/* { dg-require-effective-target mempcpy } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-chkpopt -fchkp-use-fast-string-functions" } */
/* { dg-final { scan-tree-dump "mempcpy_nobnd" "chkpopt" } } */

#define USE_GNU
#include "../../gcc.dg/strlenopt.h"

void test (int *buf1, int *buf2, size_t len)
{
  mempcpy (buf1, buf2, len);
}
