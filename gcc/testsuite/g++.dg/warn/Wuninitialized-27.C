// PR c++/19808
// { dg-do compile }
// { dg-options "-Wall" }

enum E { red };

struct C {
  C(int *, unsigned);
};

template <unsigned U> struct D : C {
  D(int, int, E) : C(e, U) {}
  int e[2];
};

void
g ()
{
  D<1>(0, 0, red);
}
