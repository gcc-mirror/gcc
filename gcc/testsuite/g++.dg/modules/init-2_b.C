// { dg-additional-options "-fmodules-ts -fno-inline" }
export module Bar;
// { dg-module-cmi Bar }

import Foo;

// { dg-final { scan-assembler {_?_ZGIW3BarEv:} } }
// { dg-final { scan-assembler {call[ \t]+_?_ZGIW3FooEv} { target i?86-*-* x86_64-*-* } } }
