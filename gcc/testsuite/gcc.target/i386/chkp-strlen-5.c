/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-strlen" } */
/* { dg-final { scan-tree-dump-times "strlen" 2 "strlen" } } */
/* { dg-final { scan-tree-dump "memcpy" "strlen" } } */

#include "../../gcc.dg/strlenopt.h"

size_t test (char *str1, char *str2)
{
  size_t len1 = strlen (str1);
  size_t len2 = strlen (str2);
  strcat (str1, str2);
  return len1 + len2 + strlen (str1);
}
