
// MS allows more things to be pointers to member functions
// { dg-options "-fms-extensions" }

struct X
{
  void Foo (X *);
  void Bar ();
};

void Quux (void (X::*) ());

void X::Foo (X *ptr)  // { dg-message "candidate" }
{
  Quux (Foo); // { dg-error "no matches" }
  Quux (Bar);
}
