/* PR tree-optimization/97260 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */

int
foo (void)
{
  const char a[] = "1234";
  return __builtin_memcmp (a, "1234", 4);
}
