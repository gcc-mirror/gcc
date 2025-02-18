/* { dg-options "-fcx-limited-range" } */

_Complex double bar (_Complex double x, _Complex double y)
{
  return x / y;
}
