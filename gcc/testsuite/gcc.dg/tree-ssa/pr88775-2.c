/* PR tree-optimization/88775 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* These can't be decided until we know how the variables will
   be laid out in memory.  */
/* { dg-final { scan-tree-dump-not "return 1;" "optimized" } } */

int a[64] = {};
int b[64] = {};
int e[0] = {};
int f[0] = {};

int
f1 (void)
{
  return (__UINTPTR_TYPE__) &a[0] != (__UINTPTR_TYPE__) &b[64];
}

int
f2 (void)
{
  return (__UINTPTR_TYPE__) &a[64] != (__UINTPTR_TYPE__) &b[0];
}

int
f3 (void)
{
  return (__UINTPTR_TYPE__) &e[0] != (__UINTPTR_TYPE__) &f[0];
}

int
f4 (void)
{
  int c[64] = {}, d[64] = {};
  return (__UINTPTR_TYPE__) &c[0] != (__UINTPTR_TYPE__) &d[64];
}

int
f5 (void)
{
  int c[64] = {}, d[64] = {};
  return (__UINTPTR_TYPE__) &c[64] != (__UINTPTR_TYPE__) &d[0];
}

/* { dg-prune-output "-Wreturn-local-addr" } */
