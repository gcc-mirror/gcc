/* PR c++/89074 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "return 1;" "optimized" } } */

int
foo (void)
{
  const char *a = &"foo"[0];
  const char *b = "foo";
  return a == b;
}
