/* PR tree-optimization/83198 */
/* { dg-do compile { target __float128 } } */
/* { dg-options "-O2 -fprintf-return-value -Wno-format -fdump-tree-optimized" } */
/* { dg-add-options __float128 } */

void bar (void);
void link_error (void);

void
foo (char *x)
{
  int a = __builtin_sprintf (x, "%f", 1.0Q);
  if (a < 8)
    link_error ();
  if (a > 13)
    bar ();
  if (a > 322)
    link_error ();
}

/* Verify we don't optimize return value to [8, 13].  */
/* { dg-final { scan-tree-dump-not "link_error \\(\\);" "optimized" } } */
/* { dg-final { scan-tree-dump "bar \\(\\);" "optimized" } } */
