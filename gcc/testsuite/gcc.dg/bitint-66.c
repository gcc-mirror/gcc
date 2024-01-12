/* PR c/113315 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

extern int a[5];

int
foo (void)
{
  _BitInt(535) i = 1;
  return a[i];
}
