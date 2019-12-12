/* PR tree-optimization/89500 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 10;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 5;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "strnlen " 1 "optimized" } } */

#include "strlenopt.h"

void foo (char *);

size_t
f1 (void)
{
  char a[10] = "0123456789";
  return strnlen (a, 10);
}

size_t
f2 (void)
{
  char a[10] = "0123456789";
  return strnlen (a, 5);
}

size_t
f3 (void)
{
  char a[10] = "0123456789";
  return strnlen (a, 0);
}

size_t
f4 (void)
{
  char a[20];
  foo (a);
  memcpy (a, "0123456789", 10);
  return strnlen (a, 10);
}

size_t
f5 (void)
{
  char a[20];
  foo (a);
  memcpy (a, "0123456789", 10);
  return strnlen (a, 14);
}

size_t
f6 (void)
{
  char a[20];
  foo (a);
  return strnlen (a, 0);
}
