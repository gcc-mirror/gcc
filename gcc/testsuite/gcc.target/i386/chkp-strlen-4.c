/* { dg-do compile { target { ! x32 } } } */
/* { dg-require-effective-target mempcpy } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-strlen" } */
/* { dg-final { scan-tree-dump-times "strlen" 1 "strlen" } } */

#define USE_GNU
#include "../../gcc.dg/strlenopt.h"

char * test (char *str1, char *str2)
{
  size_t len = strlen (str2);
  char *p = (char *)mempcpy (str1, str2, len + 1);
  return p + len + strlen (str1);
}
