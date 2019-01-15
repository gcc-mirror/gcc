/* PR tree-optimization/88775 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 1;" 10 "optimized" } } */

int a[64] = {};
int b[64] = {};

int
f1 (void)
{
  return (__UINTPTR_TYPE__) &a[2] != (__UINTPTR_TYPE__) &b[2];
}

int
f2 (void)
{
  return (__UINTPTR_TYPE__) &a[2] != (__UINTPTR_TYPE__) &b[10];
}

int
f3 (void)
{
  return (__UINTPTR_TYPE__) &a[0] != (__UINTPTR_TYPE__) &b[0];
}

int
f4 (void)
{
  return (__UINTPTR_TYPE__) &a[64] != (__UINTPTR_TYPE__) &b[64];
}

int
f5 (void)
{
  int c[64] = {};
  return (__UINTPTR_TYPE__) &a[0] != (__UINTPTR_TYPE__) &c[64];
}

int
f6 (void)
{
  int c[64] = {};
  return (__UINTPTR_TYPE__) &b[64] != (__UINTPTR_TYPE__) &c[0];
}

int
f7 (void)
{
  int c[64] = {}, d[64] = {};
  return (__UINTPTR_TYPE__) &c[2] != (__UINTPTR_TYPE__) &d[2];
}

int
f8 (void)
{
  int c[64] = {}, d[64] = {};
  return (__UINTPTR_TYPE__) &c[2] != (__UINTPTR_TYPE__) &d[10];
}

int
f9 (void)
{
  int c[64] = {}, d[64] = {};
  return (__UINTPTR_TYPE__) &c[0] != (__UINTPTR_TYPE__) &d[0];
}

int
f10 (void)
{
  int c[64] = {}, d[64] = {};
  return (__UINTPTR_TYPE__) &c[64] != (__UINTPTR_TYPE__) &d[64];
}
