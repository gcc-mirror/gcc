// Verify that -fsanitize=vptr downcast instrumentation works properly
// inside of constexpr.
// { dg-do compile }
// { dg-options "-std=c++11 -fsanitize=vptr" }

struct S {
  constexpr S() : a(0) {}
  int a;
  int f() { return 0; }
  virtual int v() { return 0; }
};

struct T : S {
  constexpr T() : b(0) {}
  int b;
  int g() { return 0; }
  virtual int v() { return 1; }
  constexpr const T *foo() { return (const T *) reinterpret_cast<const S *> (this); } // { dg-error "is not a constant expression" }
};

constexpr T t;
constexpr const T *p = t.foo ();	// { dg-error "called in a constant expression" }

template <typename U>
struct V {
  constexpr V() : a(0) {}
  int a;
  int f() { return 0; }
  virtual int v() { return 0; }
};

template <typename U>
struct W : V<U> {
  constexpr W() : b(0) {}
  int b;
  int g() { return 0; }
  virtual int v() { return 1; }
  constexpr const W<U> *foo() { return (const W<U> *) reinterpret_cast<const V<U> *> (this); }
};

constexpr W<int> w;
constexpr const W<int> *s = w.foo ();	// { dg-error "called in a constant expression" }

template <typename U>
int foo (void)
{
  static constexpr T t;
  static constexpr const T *p = t.foo ();	// { dg-error "called in a constant expression" }
  static constexpr W<U> w;
  static constexpr const W<U> *s = w.foo ();	// { dg-error "called in a constant expression" }
  return t.b + w.b;
}

int x = foo <char> ();
