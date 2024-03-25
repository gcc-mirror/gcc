/* { dg-additional-options "-std=gnu89" } */

union foo
{
  float f;
  int i;
};

foo (int a, float c)
{
  union foo b;
  b.i = a;
  return b.f + c;
}
