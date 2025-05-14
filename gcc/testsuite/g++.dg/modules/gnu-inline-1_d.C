// PR c++/119154
// { dg-additional-options "-fmodules -fno-module-lazy" }

import foo;
void bar() {}
void decl() {}

void test_c() {
  bar();
  decl();
};

// Make sure importing a gnu_inline definition didn't stop us from emitting
// the non-gnu_inline definition we had after the module import.
// { dg-final { scan-assembler "_Z3barv:" } }
// { dg-final { scan-assembler "_Z4declv:" } }
