/* PR tree-optimization/83198 */
/* { dg-do compile { target __float128 } } */
/* { dg-options "-O2 -fprintf-return-value -Wno-format -fdump-tree-optimized" } */
/* { dg-add-options __float128 } */

void bar (void);
void link_error (void);

void
foo (char *x, double y)
{
  /* The expected result should not be constant but rather that
     of the %f directive with an unknown argument, i.e., at least
     [3, 317] (but in reality [3, 322] when taking into account
     that the decimal point can be up to MB_LEN_MAX bytes long).  */
  int a = __builtin_sprintf (x, "%f", 1.0Q);
  if (a < 3)
    link_error ();
  if (a > 13)
    bar ();
  if (a > 322)
    link_error ();
}

/* Verify we don't optimize return value to [3, 13].  */
/* { dg-final { scan-tree-dump-not "link_error \\(\\);" "optimized" } } */
/* { dg-final { scan-tree-dump "bar \\(\\);" "optimized" } } */
