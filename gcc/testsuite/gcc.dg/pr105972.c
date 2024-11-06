/* PR c/105972 */
/* { dg-do compile } */
/* { dg-options "-Wno-old-style-definition -O2 -g" } */

__attribute__((optimize (0))) int
foo (void)
{
  int
  bar (x)
    int x;
  {
    return x;
  }
  return bar (0);
}
