

void Foo (int);
void Foo (double);

namespace Y
{
  void Baz (int); // { dg-message "previous declaration" }
}

void X ()
{
  using ::Foo;
  extern void Foo (int);

  using Y::Baz;
  extern void Baz (int);  // { dg-error "conflicts with" }
}
