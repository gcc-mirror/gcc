/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-chkpopt -fchkp-use-fast-string-functions" } */
/* { dg-final { scan-tree-dump "memset_nobnd" "chkpopt" } } */
/* { dg-final { cleanup-tree-dump "chkpopt" } } */

#include "string.h"

void test (int *buf1, int c, size_t len)
{
  memset (buf1, c, len);
}
