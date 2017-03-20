// PR c++/61636
// { dg-do run { target c++14 } }

// Check we don't capture this (too) unnecessarily

struct A {
  int b ();
  void f (int) {}
  static void f (double) {}

  static void g (int) {}
  static void g (double) {}
};

struct O {
  void x (int) {}
  static void x (double) {}
};

namespace N {
  void y (double) {}
}

int Check (bool expect, unsigned size)
{
  return (expect ? sizeof (void *) : 1) != size;
}

int A::b() {
  int r = 0;

  // one of the functions is non-static
  auto l0 = [&](auto z) { f (z); };
  r += Check (true, sizeof l0);
  l0(0.0); // doesn't need this capture for A::f(double), but too late
  l0 (0); // Needs this capture for A::f(int)

  // no fn is non-static.
  auto l00 = [&](auto z) { g (z); };
  r += Check (false, sizeof l00);
  l00(0.0); 
  l00 (0);

  // sizeof isn't an evaluation context, so no this capture
  auto l1 = [&](auto z) { sizeof (f (z), 1); };
  r += Check (false, sizeof l1);
  l1(0.0); l1 (0); 

  auto l2 = [&](auto) { f (2.4); };
  auto l3 = [&](auto) { f (0); };
  l2(0); l3(0); l2(0.0); l3 (0.0);
  r += Check (false, sizeof l2);
  r += Check (true, sizeof l3);

  auto l4 = [&](auto) { O::x (2.4); };
  auto l5 = [&](auto) { N::y (2.4); };
  auto l6 = [&](auto) { };
  l4(0); l5(0); l6(0);
  l4(0.0); l5(0.0); l6(0.0);
  r += Check (false, sizeof l4);
  r += Check (false, sizeof l5);
  r += Check (false, sizeof l6);

  return r;
}

int main ()
{
  A a;

  return a.b () ? 1 : 0;
}
