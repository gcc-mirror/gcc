// { dg-additional-options -fmodules-ts }
module Foo;

// We see no frob from primary's GMF
int frob (int x)
{
  return fn (-x);
}

// We see no macro from primary's GMF
int MACRO (int i)
{
  return frob (i);
}
