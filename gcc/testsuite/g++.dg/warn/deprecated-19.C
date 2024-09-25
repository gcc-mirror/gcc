// PR c++/116636
// { dg-do compile }
// { dg-options "-pedantic -Wdeprecated" }

struct A {
  virtual int foo () = 0;
};
struct B : virtual A {
  [[deprecated]] int foo () { return 0; }	// { dg-message "declared here" }
};						// { dg-warning "C\\\+\\\+11 attributes only available with" "" { target c++98_only } .-1 }
struct C : virtual A {
  [[gnu::unavailable]] int foo () { return 0; }	// { dg-message "declared here" }
};						// { dg-warning "C\\\+\\\+11 attributes only available with" "" { target c++98_only } .-1 }

void
bar ()
{
  B b;
  b.foo ();					// { dg-warning "'virtual int B::foo\\\(\\\)' is deprecated" }
  C c;
  c.foo ();					// { dg-error "'virtual int C::foo\\\(\\\)' is unavailable" }
}
