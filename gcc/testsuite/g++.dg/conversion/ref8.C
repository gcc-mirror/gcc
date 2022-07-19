// PR c++/105766
// { dg-do compile { target c++20 } }

template<class T>
struct baz {
  baz() = default;
  baz(int) requires requires { T(0); };
};

struct foo;

struct bar {
  bar() = default;
  bar(foo&);
  bar(int);
};

struct foo {
  baz<bar> m_bars;
};

foo a;
