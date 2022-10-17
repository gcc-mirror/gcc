// { dg-additional-options "-fmodules-ts -fno-inline" }

import Foo;
import Bar;
import Baz;

// We know Bar imports Foo, so only call Bar's Global Init
// { dg-final { scan-assembler {call[ \t]+_?_ZGIW3Bar} { target i?86-*-* x86_64-*-* } } }
// { dg-final { scan-assembler-not {call[ \t]+_?_ZGIW3Foo} { target i?86-*-* x86_64-*-* } } }
// We know Baz has a nop init, so don't call it.
// { dg-final { scan-assembler-not {call[ \t]+_?_ZGIW3Baz} { target i?86-*-* x86_64-*-* } } }
