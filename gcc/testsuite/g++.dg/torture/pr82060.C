// { dg-do compile }

struct A
{
  char a[1]; // must be char array
};

struct B
{
  A& a() { return ma; } // must be accessed through a getter
  A ma;
};

struct C
{
  B& b() { return mb; } // must be accessed through a getter
  B mb;
};

struct D
{
  virtual A getA() = 0; // must be virtual
};

void
foo(D& d) // The D object must not be created locally
          // (so that getA implementation is not known at compile time?)
{
  C c;
  for (;;) // must be in a loop
    c.b().a() = d.getA();
}
