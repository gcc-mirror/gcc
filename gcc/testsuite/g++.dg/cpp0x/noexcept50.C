// PR c++/86476 - noexcept-specifier is a complete-class context
// { dg-do compile { target c++11 } }

#define SA(X) static_assert(X, #X)

struct S {
  void f1() noexcept(noexcept(i)) { }
  void f2() noexcept(noexcept(fn())) { }
  void f3() noexcept(noexcept(fnx())) { }
  void f4() noexcept(noexcept(i));
  void f5() noexcept(noexcept(fn()));
  void f6() noexcept(noexcept(fnx()));

  void f7() noexcept(1);
  void f8() noexcept(0);
  void f9() noexcept(b);
  void f10() noexcept(!b);

  int i;
  static constexpr auto b = true;
  void fny() noexcept(noexcept(fn()));
  void fn();
  void fnx() noexcept;
};

S s;
SA(noexcept(s.f1()));
SA(!noexcept(s.f2()));
SA(noexcept(s.f3()));
SA(noexcept(s.f4()));
SA(!noexcept(s.f5()));
SA(noexcept(s.f6()));
SA(noexcept(s.f7()));
SA(!noexcept(s.f8()));
SA(noexcept(s.f9()));
SA(!noexcept(s.f10()));

struct S2 {
  struct V {
    void f1() noexcept(noexcept(fn()));
    void f2() noexcept(noexcept(fnx()));
    void f3() noexcept(noexcept(fn())) { }
    void f4() noexcept(noexcept(fnx())) { }
    void fn();
    void fnx() noexcept;
  } v;
  void fn();
  void fnx();
};

S2 s2;
SA(!noexcept(s2.v.f1()));
SA(noexcept(s2.v.f2()));
SA(!noexcept(s2.v.f3()));
SA(noexcept(s2.v.f4()));

struct S3 {
  void f1() noexcept(noexcept(fn()));
  void f2() noexcept(noexcept(fnx()));
  void fn();
  void fnx() noexcept;
};

void
S3::f1() noexcept(noexcept(fn()))
{
}

void
S3::f2() noexcept(noexcept(fnx()))
{
}

struct S4 {
  int f1 (int p) noexcept(noexcept(p)) { return p; }
  int f2 (int p) noexcept(noexcept(p));
  int f3 (int p = 10) noexcept(noexcept(p));
  int f4 () noexcept(noexcept(S4{}));
};

S4 s4;
SA(noexcept(s4.f1(1)));
SA(noexcept(s4.f2(1)));
SA(noexcept(s4.f3()));
SA(noexcept(s4.f4()));

template<typename T>
struct S5 {
  void f1() noexcept(noexcept(i)) { }
  void f2() noexcept(noexcept(fn())) { }
  void f3() noexcept(noexcept(fnx())) { }
  void f4() noexcept(noexcept(i));
  void f5() noexcept(noexcept(fn()));
  void f6() noexcept(noexcept(fnx()));
    
  int i;
  void fny() noexcept(noexcept(fn()));
  void fn();
  void fnx() noexcept;
};

S5<int> s5;
SA(noexcept(s5.f1()));
SA(!noexcept(s5.f2()));
SA(noexcept(s5.f3()));
SA(noexcept(s5.f4()));
SA(!noexcept(s5.f5()));
SA(noexcept(s5.f6()));

template<typename T>
struct S6 {
  void f1() noexcept(noexcept(x));
  T x;
};

struct S7 {
  template<typename U>
  void f1 () noexcept(noexcept(U(1))) { }

  template<int N>
  void f2() noexcept(noexcept(N));

  template <typename _Up>
  void f3(_Up __p) noexcept(noexcept(__p));
};

void glob();
void globx() noexcept;
struct S8 {
  void f1 () noexcept(noexcept(glob()));
  void f2 () noexcept(noexcept(globx()));
};

S8 s8;
SA(!noexcept(s8.f1()));
SA(noexcept(s8.f2()));

struct W {
  constexpr operator bool();
};

template<typename T>
struct S9 {
  S9() noexcept(noexcept(w)) { }
  S9 &operator=(S9 &&) noexcept(T::X);
  W w;
};
