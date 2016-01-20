/* { dg-do compile { target { ! x32 } } } */
/* { dg-require-effective-target mempcpy } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-chkpopt -fchkp-use-fast-string-functions -D_GNU_SOURCE" } */
/* { dg-final { scan-tree-dump-not "mempcpy_nobnd" "chkpopt" } } */

#include "string.h"

void test (void *buf1, void *buf2, size_t len)
{
  mempcpy (buf1, buf2, len);
}
