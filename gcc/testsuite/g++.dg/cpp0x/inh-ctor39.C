// PR c++/114784
// { dg-do compile { target c++11 } }
// { dg-additional-options "-O2" }

template <typename T>
struct A {
  [[gnu::always_inline]] A (int t) { foo ().bar (t, {}); }
  [[gnu::always_inline]] A (long long t) { foo ().bar (t, {}); }
  T foo ();
};

struct B : A<B> {
  using A<B>::A;
  [[gnu::always_inline]] B (long long v) : A (v) {}
  template <typename T>
  void bar (T &&, int);
  char b;
};

struct C {
  C (int v) : a(v) { }
  C (long long v) : a(v) { }
  B a;
};

static C
baz ()
{
  C x(0);
  C y(0LL);
  return 0;
}

[[gnu::cold]] int
qux ()
{
  baz ();
  return 0;
}

template <typename>
struct D {
  template <typename T>
  [[gnu::always_inline]] D (T) { d = sizeof (T); }
  D();
  int d;
};
template <typename T>
struct E : D<T> {
  using D<T>::D;
};

E<char> c = {};
E<char> d = 1;
E<char> e = 1.0;
