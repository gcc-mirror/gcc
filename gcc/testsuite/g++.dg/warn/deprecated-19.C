// PR c++/116636
// { dg-do compile { target c++11 } }
// { dg-options "-pedantic -Wdeprecated" }

struct A {
  virtual int foo () = 0;
};
struct B : virtual A {
  [[deprecated]] int foo () { return 0; }	// { dg-message "declared here" }
};
struct C : virtual A {
  [[gnu::unavailable]] int foo () { return 0; }	// { dg-message "declared here" }
};

void
bar ()
{
  B b;
  b.foo ();					// { dg-warning "'virtual int B::foo\\\(\\\)' is deprecated" }
  C c;
  c.foo ();					// { dg-error "'virtual int C::foo\\\(\\\)' is unavailable" }
}
