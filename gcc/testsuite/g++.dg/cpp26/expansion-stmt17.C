// C++26 P1306R5 - Expansion statements
// { dg-do run { target c++11 } }
// { dg-options "" }
// { dg-additional-options "-frange-for-ext-temps" { target c++20_down } }

int c;
struct A {
  A () { ++c; }
  ~A () { --c; }
  A (int x) : a (x) { ++c; }
  A (const A &x) : a (x.a) { ++c; }
  int a;
};
struct B { int a; long b; double c; short d; };

B &
foo (A x, A y, A z)
{
  static B r = { 1, 2, 42, 3 };
  return r;
}

int
main ()
{
  int r = 0;
  if (c != 0)
    __builtin_abort ();
  template for (auto a : foo (A { 1 }, A { 2 }, A { 42 }))	// { dg-warning "'template for' only available with" "" { target c++23_down } }
    {
      if (c != 3)
	__builtin_abort ();
      r += a;
    }
  if (c != 0 || r != 48)
    __builtin_abort ();
}
