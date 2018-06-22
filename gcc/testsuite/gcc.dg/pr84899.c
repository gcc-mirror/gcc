/* PR target/84899 */
/* { dg-do compile } */
/* { dg-options "-O -funroll-all-loops -fno-move-loop-invariants" } */

void
foo (int x)
{
  int a = 1 / x, b = 0;

  while ((a + b + 1) < x)
    b = __INT_MAX__;
}
