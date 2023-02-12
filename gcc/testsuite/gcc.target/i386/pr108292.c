/* PR target/108292 */
/* { dg-do compile } */
/* { dg-options "-Ofast -march=alderlake" } */

extern void foo (float *);

extern int x;

int
bar (void)
{
  float y;
  foo (&y);
  return y > x ? 1 : 2;
}
