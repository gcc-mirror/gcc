/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1" } */

#include <string.h>

char c[10];

void
f1 ()
{
  const char *p = "123456";
  memcpy (c, p, 6);
}

void
f2 ()
{
  const char *p = "12345678";
  p += 2;
  memcpy (c, p, 6);
}

/* { dg-final { scan-tree-dump-times "memcpy\[^\n\]*123456" 2 "ccp1" } } */
