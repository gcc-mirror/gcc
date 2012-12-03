/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O1" } */

int a, c;

void f(long long p)
{
  long long b;

  if(b)
    b = p ? : 0;

  for (; p; p++)
    p *= a & (c = p *= !a < 2);

  a = b += !(b & 3740917449u);
}
