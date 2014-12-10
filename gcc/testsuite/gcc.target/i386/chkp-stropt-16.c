/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-require-effective-target mempcpy } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-chkpopt -fchkp-use-nochk-string-functions -fchkp-use-fast-string-functions -D_GNU_SOURCE" } */
/* { dg-final { scan-tree-dump "mempcpy_nobnd_nochk" "chkpopt" } } */
/* { dg-final { cleanup-tree-dump "chkpopt" } } */

#include "string.h"

void test (int *buf1, int *buf2, size_t len)
{
  mempcpy (buf1, buf2, len);
}
