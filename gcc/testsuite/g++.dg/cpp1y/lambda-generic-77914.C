// PR c++/77914
// { dg-do compile { target c++14 } }

int
main ()
{
  auto l = [] <typename T> () {};	// { dg-error "does not support lambda templates" }
  l.operator () <void> ();
}
