// PR c++/118923
// { dg-do run { target c++11 } }
// { dg-additional-options "-frange-for-ext-temps" { target c++23 } }
// { dg-additional-options "-fno-range-for-ext-temps" { target c++20_down } }

int g;

struct A {
  int a[3];
  A (int x, int y, int z) : a{x, y, z} { if ((g++ & 7) != 4) __builtin_abort (); }
  A (const A &x) = delete;
  ~A () { if ((g++ & 7) != 7 - 2 * (__cpp_range_based_for >= 202211)) __builtin_abort (); }
  int *begin () { return a; }
  int *end () { return a + 3; }
};

struct B {
  B () { if ((g++ & 7) != 3) __builtin_abort (); }
  B (const B &) = delete;
  ~B () { if ((g++ & 7) != 5 + (__cpp_range_based_for >= 202211)) __builtin_abort (); }
};

struct C {
  A foo (const B &) { return { 1, 2, 3 }; }
  A bar (const B &) { return { 4, 5, 6 }; }
  bool baz () { return b; }
  bool b = false;
  static C c;
};

C C::c;

struct D {
  D () { if ((g++ & 5) != 0) __builtin_abort (); }
  D (const D &) = delete;
  ~D () { if ((g & 7) != 1 && (g & 7) != 6 + (__cpp_range_based_for >= 202211)) __builtin_abort (); g++; }
};

inline C *
qux (const D &)
{
  return &C::c;
}

void 
foo ()
{
  int z = 1;
  auto d = qux (D {})->baz () ? &C::bar : &C::foo;
  for (const int &r : (qux (D {})->*d) (B {}))
    if (z++ != r)
      __builtin_abort ();
  C::c.b = true;
  d = qux (D {})->baz () ? &C::bar : &C::foo;
  for (const int &r : (qux (D {})->*d) (B {}))
    if (z++ != r)
      __builtin_abort ();
}

int
main ()
{
  foo ();
  if (g != 16)
    __builtin_abort ();
}
