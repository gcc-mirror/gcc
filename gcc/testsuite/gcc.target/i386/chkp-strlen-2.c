/* { dg-do compile { target { ! x32 } } } */
/* { dg-require-effective-target stpcpy } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-strlen" } */
/* { dg-final { scan-tree-dump-not "strlen" "strlen" } } */

#define USE_GNU
#include "../../gcc.dg/strlenopt.h"

char *test (char *str1, char *str2)
{
  char *p = stpcpy (str1, str2);
  size_t len = strlen (str1);
  return p + len;
}
