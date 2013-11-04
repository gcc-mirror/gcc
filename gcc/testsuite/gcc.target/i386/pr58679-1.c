/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

typedef struct { char a; long long b; } S;

S foo (S x, S y)
{
  S z;

  z.a = 0;
  z.b = x.b / y.b;
  return z;
}
