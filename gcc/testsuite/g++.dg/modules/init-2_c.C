// { dg-additional-options "-fmodules-ts -fno-inline" }
export module Baz;
// { dg-module-cmi Baz }

// { dg-final { scan-assembler {_ZGIW3Baz:} } }
// But it is empty, and so no idempotency bool
// { dg-final { scan-assembler-not {_ZZ9_ZGIW3BazE9__in_chrg} } }
