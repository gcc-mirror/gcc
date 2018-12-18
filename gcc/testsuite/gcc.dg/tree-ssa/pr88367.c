/* PR c/88367 */
/* { dg-do compile } */
/* { dg-options "-fno-delete-null-pointer-checks -O2 -fdump-tree-optimized -fno-wrapv-pointer" } */
/* { dg-final { scan-tree-dump-not "link_error \\(\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-times "bar \\(\\);" 2 "optimized" } } */

void bar (void);
void link_error (void);

void
foo (char *p)
{
  if (!p)
    return;
  p += 3;
  if (!p)
    link_error ();
  p -= 6;
  if (!p)
    bar ();
}

void
baz (char *p)
{
  if (!p)
    return;
  p -= 6;
  if (!p)
    bar ();
}
