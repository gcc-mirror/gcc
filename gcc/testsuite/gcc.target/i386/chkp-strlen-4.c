/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-strlen -D_GNU_SOURCE" } */
/* { dg-final { scan-tree-dump-times "strlen" 1 "strlen" } } */
/* { dg-final { cleanup-tree-dump "strlen" } } */

#include "string.h"

char * test (char *str1, char *str2)
{
  size_t len = strlen (str2);
  char *p = (char *)mempcpy (str1, str2, len + 1);
  return p + len + strlen (str1);
}
