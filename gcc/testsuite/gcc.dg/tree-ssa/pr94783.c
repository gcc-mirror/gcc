/* PR tree-optimization/94783 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "ABS_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump-not " >> 31" "optimized" } } */

int
foo (int v)
{
  int mask = v >> (__SIZEOF_INT__ * __CHAR_BIT__ - 1);
  return (v + mask) ^ mask;
}
