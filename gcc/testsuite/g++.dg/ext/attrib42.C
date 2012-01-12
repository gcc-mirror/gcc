// { dg-do compile { target i?86-*-* } }

struct A {
  __attribute__((fastcall))
  void f();
};

int main()
{
    typedef void (A::*FP)();
    FP fp[] = {&A::f};		// { dg-error "cannot convert" }
}
