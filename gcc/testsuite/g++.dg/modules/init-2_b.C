// { dg-additional-options "-fmodules-ts -fno-inline" }
export module Bar;
// { dg-module-cmi Bar }

import Foo;

// { dg-final { scan-assembler {_?_ZGIW3Bar:} } }
// There should be an idempotency check
// { dg-final { scan-assembler {_ZZ9_ZGIW3BarE9__in_chrg} } }
// { dg-final { scan-assembler {call[ \t]+_?_ZGIW3Foo} { target i?86-*-* x86_64-*-* } } }
// { dg-final { scan-assembler {.(quad|long)[ \t]+_?_ZGIW3Bar} { target i?86-*-* x86_64-*-* } } }
