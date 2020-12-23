// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fno-inline" }
export module Foo;
// { dg-module-cmi Foo }

int Frob (int i)
{
  return i;
}

export int j = Frob (5);
