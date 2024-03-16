/* { dg-additional-options "-std=gnu89" } */

inline f(i)
{
  h((long long) i * 2);
}
g()
{
  f(9);
}
