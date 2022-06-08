// { dg-additional-options "-fmodules-ts -fno-inline" }
export module Foo;
// { dg-module-cmi Foo }

// { dg-final { scan-assembler {_ZGIW3Foo:} } }
// But it is empty, and so no idempotency bool
// { dg-final { scan-assembler-not {_ZZ9_ZGIW3FooE9__in_chrg} } }
