// PR c++/119154
// { dg-additional-options "-fmodules" }

void bar();
void decl();  // { dg-warning "used but never defined" }
import foo;

void test_b() {
  bar();
  decl();
}

// A function only defined with gnu_inline should not be emitted here.
// { dg-final { scan-assembler-not "_Z3barv:" } }
