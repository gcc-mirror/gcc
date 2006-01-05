// { dg-require-weak "" }
// { dg-final { scan-assembler ".weak\[ \t\]_?_ZThn._N7Derived3FooEv" } }

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
