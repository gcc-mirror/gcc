// { dg-additional-options "-fmodules-ts -fno-inline" }
export module Foo;
// { dg-module-cmi Foo }

// { dg-final { scan-assembler {_ZGIW3FooEv:} } }
