// { dg-require-weak "" }
// { dg-skip-if "Linkonce not weak" { *-*-mingw* *-*-cygwin } { "*" } { "" } }
// { dg-final { scan-assembler ".weak\[ \t\]_?_ZThn._N7Derived3FooEv" { target { ! { *-*-darwin* alpha*-dec-osf* } } } } }
// { dg-final { scan-assembler ".weak_definition\[ \t\]_?_ZThn._N7Derived3FooEv" { target { *-*-darwin* } } } }
// { dg-final { scan-assembler ".weakext\[ \t\]_?_ZThn._N7Derived3FooEv" { target { alpha*-dec-osf* } } } }

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
