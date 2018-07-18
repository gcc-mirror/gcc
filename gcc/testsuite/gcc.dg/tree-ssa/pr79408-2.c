/* PR tree-optimization/79408 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void link_error (void);

void
foo (unsigned int y)
{
  if (y <= 7312)
    return;
  if (7312 % y != 7312)
    link_error ();
}

void
bar (int x, int y)
{
  if (y <= 7312)
    return;
  if (7312 % y != 7312)
    link_error ();
}

void
baz (int x, int y)
{
  if (y <= 7312)
    return;
  if (-7312 % y != -7312)
    link_error ();
}

/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
