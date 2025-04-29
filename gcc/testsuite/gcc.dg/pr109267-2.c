/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR middle-end/109267 */
void g(void);
int f(int *t)
{
  g();
  __builtin_unreachable();
}

/* The unreachable should stay a unreachable. */
/* { dg-final { scan-tree-dump-not "__builtin_unreachable trap \\\(" "optimized"} } */
/* { dg-final { scan-tree-dump-times "__builtin_unreachable \\\(" 1 "optimized"} } */
