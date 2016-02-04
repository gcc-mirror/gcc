/* { dg-do compile { target { ! x32 } } } */
/* { dg-require-effective-target stpcpy } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -fdump-tree-strlen" } */
/* { dg-final { scan-tree-dump-not "strlen" "strlen" } } */

#define _GNU_SOURCE
#include "string.h"

char *test (char *str1, char *str2)
{
  char *p = stpcpy (str1, str2);
  size_t len = strlen (str1);
  return p + len;
}
