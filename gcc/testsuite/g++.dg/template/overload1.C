// { dg-options "-w" }
// PR c++/9420
// Bug: We were instantiating B<int> during overload resolution for E<0.
// This is wrong; the contents of B<int> are not relevant, since we can't
// use its constructors (because we'd already be using a constructor for
// C).

enum { E };

template <typename T> struct A {
  static const int a = (E < 0);
};

template <typename T> class B {
  A<int> b;
};

struct C {
  C(B<int>);
};

int operator<(C, C);

A<int> c;
