// PR c++/58678
// { dg-options "-O3 -fdump-ipa-devirt" }

struct A {
  virtual ~A();
};
struct B : A {
  virtual int m_fn1();
};
void fn1(B* b) {
  delete b;
}

// { dg-final { scan-assembler-not "_ZN1AD2Ev" } }
// { dg-final { scan-assembler-not "_ZN1BD0Ev" } }
// { dg-final { scan-ipa-dump "Target is artificial" "devirt" } }
