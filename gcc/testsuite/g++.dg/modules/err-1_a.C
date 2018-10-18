// { dg-additional-options "-fmodules-ts" }
export module Foo;
// { dg-module-bmi "Foo" }

export int Frob (int, int, long);
export int Frob (int, long, int);
