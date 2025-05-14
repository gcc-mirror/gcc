// PR c++/119154
// { dg-additional-options "-fmodules" }

void bar() {}
void decl() {}
import foo;

void test_c() {
  bar();
  decl();
};

// Make sure importing a gnu_inline definition didn't stop us from emitting
// the non-gnu_inline definition we had before the module import.
// { dg-final { scan-assembler "_Z3barv:" } }
// { dg-final { scan-assembler "_Z4declv:" } }
