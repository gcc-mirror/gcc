/* PR tree-optimization/92056 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 1;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "strcmp \\(" "optimized" } } */

void bar (int, char *);

int
foo (int x, char *y, const char *z)
{
  char *a;
  __builtin_sprintf (y, z);
  if (x == 3)
    a = __builtin_malloc (5);
  else if (x == 7)
    a = __builtin_malloc (6);
  else
    a = __builtin_malloc (7);
  bar (x, a);
  return __builtin_strcmp (a, "abcdefg") != 0;
}

int
baz (int x)
{
  char *a;
  if (x == 3)
    a = __builtin_malloc (5);
  else if (x == 7)
    a = __builtin_malloc (6);
  else
    a = __builtin_malloc (7);
  bar (x, a);
  return __builtin_strcmp (a, "abcdefg") != 0;
}
