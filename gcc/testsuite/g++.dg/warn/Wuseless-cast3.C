// PR c++/85043
// { dg-do compile { target c++11 } }
// { dg-options "-Wuseless-cast" }

struct S { int s; void bump () { s++; } };

void
foo ()
{
  S s = { 1 };
  s.bump ();
  S (s).bump (); // { dg-bogus "useless" }
  ((S) s).bump (); // { dg-bogus "useless" }
  static_cast<S> (s).bump (); // { dg-bogus "useless" }
}

struct X { };
void g(X&&);

void
f (X&& arg)
{
  g(X(arg)); // { dg-bogus "useless" }
  g(X(X{})); // { dg-warning "useless" }
  g(static_cast<X&&>(arg));

  int i = (int) 1; // { dg-warning "useless" }
  const int &r = (int) i; // { dg-bogus "useless" }
}
