// { dg-additional-options "-fmodules-ts" }
// { dg-module-do run }

export module Foo;
// { dg-module-cmi "Foo" }

export int bob (int);
export float bob (float);
