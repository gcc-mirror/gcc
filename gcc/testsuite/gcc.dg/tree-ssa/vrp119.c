/* PR tree-optimization/86231 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */
/* { dg-final { scan-tree-dump-not "link_error" "vrp1" } } */

int bar (int);
void link_error (void);

int
foo (int x, int y, int z)
{
  if (x < 4 || x > 8) __builtin_unreachable ();
  if (y >= 2 && y <= 6) __builtin_unreachable ();
  /* x is [4, 8], y is ~[2, 6], resulting range of e should be ~[2, 3].  */
  int e = (z ? x : y);
  bar (bar (bar (bar (bar (bar (bar (bar (bar (bar (bar (bar (e))))))))))));
  if (e == 2 || e == 3)
    link_error ();
  return e;
}
