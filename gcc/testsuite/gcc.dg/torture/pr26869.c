/* { dg-do compile } */

_Complex float f (_Complex float b, _Complex float c)
{
  _Complex float a = 1.0 + 0.0i;
  return a / c;
}
