/* PR inline-asm/39058 */
/* { dg-options "-O2" } */

double
f1 ()
{
  double x;
  asm ("" : "=r,r" (x) : "0,0" (x));
  return x;
}

double
f2 ()
{
  double x;
  asm ("" : "=r" (x) : "0" (x));
  return x;
}

double
f3 ()
{
  double x, y;
  asm ("" : "=r,r" (x), "=r,r" (y) : "%0,0" (x), "r,r" (0));
  return x;
}

double
f4 ()
{
  double x, y;
  asm ("" : "=r" (x), "=r" (y) : "0" (x), "r" (0));
  return x;
}
