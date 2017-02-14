/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O -mbmi2" } */

unsigned long long a;
int b;

int
fn1(int p1)
{
  p1 &= 1;
  p1 &= (short)~p1;
  b = a;
  a = a << p1 | a >> (64 - p1);
  return p1 + 1 + a;
}
