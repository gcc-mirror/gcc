/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/111432 */

int
foo3(int c, int bb)
{
  if ((bb & ~3)!=0) __builtin_unreachable();
  return (bb & (c|3));
}

int
foo_bool(int c, _Bool bb)
{
  return (bb & (c|7));
}

/* Both of these functions should be able to remove the `IOR` and `AND`
   as the only bits that are non-zero for bb is set on the other side
   of the `AND`.
 */

/* { dg-final { scan-tree-dump-not   "bit_ior_expr, "   "optimized" } } */
/* { dg-final { scan-tree-dump-not   "bit_and_expr, "   "optimized" } } */
