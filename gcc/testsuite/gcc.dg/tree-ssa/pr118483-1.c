/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/118483 */
/* { dg-final { scan-tree-dump-not "abort " "optimized" } } */


/* The value of `l == e` is always false as it is
   `(b == 0) == (b != 0)`. */

int d;
int f(int b)
{
  int e = b == 0;
  d = e;
  int l = b != 0;
  if (l == e)
    __builtin_abort ();
}
