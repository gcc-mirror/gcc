/* PR c/86093 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "return 0;" "optimized" } } */

char *volatile p;

__PTRDIFF_TYPE__
foo (void)
{
  return p - p;
}
