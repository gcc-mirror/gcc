// { dg-additional-options "-fmodules-ts" }
module Foo;

int bob (int a)
{
  return a * 2;
}

float bob (float b)
{
  return b * 1.5f;
}
