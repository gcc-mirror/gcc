// { dg-options "-pedantic" }
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }
// { dg-require-effective-target c++11 }

struct A {
  [[gnu::fastcall]]
  void f();
};

int main()
{
    typedef void (A::*FP)();
    FP fp[] = {&A::f};		// { dg-error "cannot convert" }
}
