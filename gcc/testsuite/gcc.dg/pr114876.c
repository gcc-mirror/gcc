/* PR tree-optimization/114876 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "return \[01\];" "optimized" } } */
/* { dg-final { scan-tree-dump "return 3;" "optimized" } } */
/* { dg-final { scan-tree-dump "return 4;" "optimized" } } */

int
foo (void)
{
  char buf[64];
  return __builtin_sprintf (buf, "%lc%lc%lc", (__WINT_TYPE__) 0, (__WINT_TYPE__) 0, (__WINT_TYPE__) 0);
}

int
bar (void)
{
  char buf[64];
  return __builtin_sprintf (buf, "%c%c%c", 0, 0, 0);
}

int
baz (void)
{
  char buf[64];
  return __builtin_sprintf (buf, "%lc%lc%lca", (__WINT_TYPE__) 0, (__WINT_TYPE__) 0, (__WINT_TYPE__) 0);
}

int
qux (void)
{
  char buf[64];
  return __builtin_sprintf (buf, "%c%c%ca", 0, 0, 0);
}
