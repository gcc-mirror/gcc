// { dg-additional-options "-fmodules-ts" }
export module Foo;
// { dg-module-cmi "Foo" }

export int Frob (int, int, long);
export int Frob (int, long, int);
