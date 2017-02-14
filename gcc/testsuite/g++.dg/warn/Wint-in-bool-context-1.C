// PR c++/79184
// { dg-do compile }
// { dg-options "-Wint-in-bool-context" }

enum { E = 2 };
template <bool> void f(int) { }
template <int> void f() {}

int
main ()
{
  f<1 * 1>(); // { dg-bogus "in boolean context" }
  f<1 << 1>(); // { dg-bogus "in boolean context" }
  f<1 ? 3 : 2>(); // { dg-bogus "in boolean context" }
  f<E>(); // { dg-bogus "in boolean context" }
}
