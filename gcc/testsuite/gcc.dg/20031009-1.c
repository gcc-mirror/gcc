/* PR optimization/12510 */
/* Origin: Lars Skovlund <lskovlun@image.dk>  */
/* Reduced testcase by Volker Reichelt <reichelt@igpm.rwth-aachen.de> */

/* Verify that one splitting pass is not missing on x86 at -O1 */

/* { dg-do compile } */
/* { dg-options "-O -mcpu=i686" { target i?86-*-* } } */

extern foo(double);

void bar(double x, double y)
{
  foo (x);
  if (y) x = y ? 0 : 1/y;
  else if (y) x = y < 1 ? 1 : y;
  else x = 1/y < 1 ? 1 : x;
  foo (x);
}
