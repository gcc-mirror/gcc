/* { dg-do compile } */
/* { dg-options "-O -ffast-math" } */
_Complex float f (_Complex float a)
{
  _Complex float b = a / a;
  return b;
}
