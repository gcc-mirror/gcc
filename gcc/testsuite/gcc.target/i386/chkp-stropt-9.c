/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-chkpopt -fchkp-use-fast-string-functions" } */
/* { dg-final { scan-tree-dump-not "memcpy_nobnd" "chkpopt" } } */
/* { dg-final { cleanup-tree-dump "chkpopt" } } */

#include "string.h"

void test (void *buf1, void *buf2, size_t len)
{
  memcpy (buf1, buf2, len);
}
