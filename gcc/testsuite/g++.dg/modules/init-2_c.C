// { dg-additional-options "-fmodules-ts -fno-inline" }

import Foo;
import Bar;

// We know Bar imports Foo, so only call Bar's Global Init
// { dg-final { scan-assembler {call[ \t]+_?_ZGIW3BarEv} { target i?86-*-* x86_64-*-* } } }
// { dg-final { scan-assembler-not {call[ \t]+_?_ZGIW3FooEv} { target i?86-*-* x86_64-*-* } } }
