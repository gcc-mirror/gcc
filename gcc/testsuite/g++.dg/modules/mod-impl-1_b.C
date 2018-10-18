// { dg-additional-options "-fmodules-ts" }
module baz;

int Square (int a)
{
  return Prod (a, a);
}

float Square (int a, int b)
{
  return a * b * 1.5f;
}
