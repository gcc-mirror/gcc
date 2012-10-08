// { dg-options "-std=c++11 -pedantic" }
// { dg-do compile { target { i?86-*-* && ilp32 } } }

struct A {
  [[gnu::fastcall]]
  void f();
};

int main()
{
    typedef void (A::*FP)();
    FP fp[] = {&A::f};		// { dg-error "cannot convert" }
}
