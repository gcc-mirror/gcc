// { dg-final { scan-assembler ".weak\t_ZThn._N7Derived3FooEv" } }

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

inline void Derived::Foo ()
{
}

Derived f;
