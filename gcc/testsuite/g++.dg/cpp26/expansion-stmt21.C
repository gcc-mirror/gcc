// DR3048 - Empty destructuring expansion statements
// { dg-do run { target c++11 } }
// { dg-options "" }

struct A {};

int
foo ()
{
  int c[0] = {};
  int r = 0;
  template for (auto a : A {})		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ++r;
  template for (int i : c)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ++r;
  return r;
}

int
main ()
{
  if (foo () != 0)
    __builtin_abort ();
}
