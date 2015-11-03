/* { dg-do compile } */
/* { dg-options "-g" } */

long long a;

short
fn1 (short p1, unsigned short p2)
{
  return p1 + p2;
}

short
fn2 ()
{
  int b = a ? fn1 (fn2 (), a) : 0;
  return b;
}
