// PR c++/77914
// { dg-do compile { target c++14 } }

int
main ()
{
  auto l = [] <typename T> () {};	// { dg-error "lambda templates are only available with" "" { target c++17_down } }
  l.operator () <void> ();
}
