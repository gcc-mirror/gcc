/* PR tree-optimization/63464 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void bar (void);

void
foo (int x)
{
  if (x != 2 && x != 3 && x != 10 && x != 11 && x != 17 && x != 18 && x != 23)
    bar ();
}

/* Check if the tests have been folded into a bit test.  */
/* { dg-final { scan-tree-dump "(8784908|-8784909|0x0*860c0c)" "optimized" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "(<<|>>)" "optimized" { target i?86-*-* x86_64-*-* } } } */
