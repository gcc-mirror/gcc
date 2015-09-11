/* PR tree-optimization/63464 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void bar (void);

void
foo (int x)
{
  if (x == 43 || x == 76 || x == 44 || x == 78 || x == 49
      || x == 77 || x == 46 || x == 75 || x == 45 || x == 82)
    bar ();
}

/* Check if the tests have been folded into a bit test.  */
/* { dg-final { scan-tree-dump "(614180323407|0x0*8f0000004f)" "optimized" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "(<<|>>)" "optimized" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
