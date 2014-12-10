/* PR target/63618 */
/* { dg-do run } */
/* { dg-options "-O2 -mtune=corei7 -fno-inline" } */
/* { dg-additional-options "-msse4.2" { target sse4 } } */
/* { dg-additional-options "-fpic" { target fpic } } */

static const __float128 cf = 0.1E+10Q;

__float128
f128_square(__float128 x)
{
  return x * x;
}

__float128
f128_p3(__float128 x)
{
  return x * x * x;
}

__float128
cond_f128_square (__float128 x, int p)
{
  x = f128_p3 (x);
  if (p)
    x = f128_square(cf);
  return x;
}

int main()
{
  __float128 x = cond_f128_square (cf, 1);
  return (int)(x < cf);
}
