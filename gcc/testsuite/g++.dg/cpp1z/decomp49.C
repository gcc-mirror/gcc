// PR c++/82075
// { dg-do run { target c++11 } }
// { dg-options "" }

struct B { };
struct D : B { int i; };

int
main ()
{
  auto [i] = D{};	// { dg-warning "only available with" "" { target c++14_down } }
  if (i != 0)
    __builtin_abort ();
}
