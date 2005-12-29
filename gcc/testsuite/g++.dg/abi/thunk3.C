// { dg-final { scan-assembler-not ".weak\t_ZThn4_N7Derived3FooEv" } }

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
