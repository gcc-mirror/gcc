/* { dg-do compile } */

_Complex float f(_Complex float a, float b)
{
  return a - a*b;
}
