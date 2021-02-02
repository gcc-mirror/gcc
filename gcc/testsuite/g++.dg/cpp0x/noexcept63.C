// PR c++/97966
// { dg-do compile { target c++11 } }

template <int>
struct S1 {
  __attribute__((used)) S1() noexcept(noexcept(this->foo())) { }
  void foo();
};

template <int>
struct S2 {
  __attribute__((used)) void bar() noexcept(noexcept(this->foo())) { }
  void foo();
};

template <int>
struct S3 {
  void __attribute__((used)) bar() noexcept(noexcept(this->foo())) { }
  void foo();
};

template <int>
struct S4 {
  [[gnu::used]] void bar() noexcept(noexcept(this->foo())) { }
  void foo();
};

template <int>
struct S5 {
  void bar() noexcept(noexcept(this->foo())) __attribute__((used)) { }
  void foo();
};

template <int>
struct S6 {
  template <int>
  struct N {
    [[gnu::used]] void bar() noexcept(noexcept(this->foo())) { }
    void foo();
  };
};

void
g ()
{
  S1<1> s1;
  S2<1> s2;
  S3<1> s3;
  S4<1> s4;
  S5<1> s5;
  S6<1>::N<1> n;
}

// Make sure that we did emit the functions marked with attribute used
// even though they're not referenced in this TU.  (Well, the S1()
// constructor is.)
// { dg-final { scan-assembler "_ZN2S1ILi1EEC1Ev" } }
// { dg-final { scan-assembler "_ZN2S1ILi1EEC2Ev" } }
// { dg-final { scan-assembler "_ZN2S2ILi1EE3barEv" } }
// { dg-final { scan-assembler "_ZN2S3ILi1EE3barEv" } }
// { dg-final { scan-assembler "_ZN2S4ILi1EE3barEv" } }
// { dg-final { scan-assembler "_ZN2S5ILi1EE3barEv" } }
// { dg-final { scan-assembler "_ZN2S6ILi1EE1NILi1EE3barEv" } }
