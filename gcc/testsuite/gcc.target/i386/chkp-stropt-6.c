/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-chkpopt -fchkp-use-fast-string-functions" } */
/* { dg-final { scan-tree-dump "memset_nobnd" "chkpopt" } } */

#include "../../gcc.dg/strlenopt.h"

void test (int *buf1, int c, size_t len)
{
  memset (buf1, c, len);
}
