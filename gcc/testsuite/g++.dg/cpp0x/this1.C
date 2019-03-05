// PR c++/88548
// { dg-do compile { target c++11 } }

struct S1 {
  int a;
  auto m1 () -> decltype(this->a) { return 0; }
  auto m2 () -> decltype(this) { return 0; }
  void m3 () noexcept(noexcept(this->a)) { }
  void m4 () noexcept(noexcept(this)) { }

  static auto m5 () -> decltype(this->a) { return 0; } // { dg-error ".this. may not be used in this context" }
  static auto m6 () -> decltype(this) { return 0; } // { dg-error ".this. may not be used in this context" }
  static void m7 () noexcept(noexcept(this->a)) { } // { dg-error ".this. may not be used in this context" }
  static void m8 () noexcept(noexcept(this)) { } // { dg-error ".this. may not be used in this context" }
};

template <typename T>
struct S2 {
  static auto f1(T arg) -> decltype((arg));
};

struct S3 {
  int a;
  void f1 () noexcept(noexcept(a)) { }
  static void f2() noexcept(noexcept(a)) { }
  static auto f3() -> decltype(a);
  static auto f4() -> decltype((a));
};

template<typename T>
class S4 {
  T i;
  friend int foo(const S4 &t) noexcept(noexcept(i)) { return t.i; }
};

void
test ()
{
  S4<int> t;
  foo(t);
}

struct S5 {
  friend auto bar() -> decltype(this); // { dg-error ".this. may not be used in this context" }
  auto bar2() -> decltype(this);
};
