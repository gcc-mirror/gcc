// { dg-require-weak "" }
// { dg-final { scan-assembler-not ".weak\[\t \]_?_ZThn._N7Derived3FooEv" { target { ! { *-*-darwin* } } } } }
// { dg-final { scan-assembler-not ".weak_definition\[\t \]_?_ZThn._N7Derived3FooEv" { target { *-*-darwin* } } } }

struct Base 
{
  virtual void Foo ();
};

struct Filler 
{
  virtual void Baz ();
};

struct Derived : Filler, Base 
{
  virtual void Foo ();
};

void Derived::Foo ()
{
}
