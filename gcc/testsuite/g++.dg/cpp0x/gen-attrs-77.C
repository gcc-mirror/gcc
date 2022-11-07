// PR c++/106712
// { dg-do compile { target c++11 } }

[[noreturn]] int f1 [[nodiscard]](), f2 ();
[[nodiscard]] int f3 (), f4 ();
int f5 [[nodiscard]](), f6 ();

int
main ()
{
  f1 (); // { dg-warning "ignoring" }
  f2 ();
  f3 (); // { dg-warning "ignoring" }
  f4 (); // { dg-warning "ignoring" }
  f5 (); // { dg-warning "ignoring" }
  f6 ();
}
