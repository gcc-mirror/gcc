// PR c++/48035
// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cstring" { ! hostedlib } }

#include <new>
#include <cstring>
#include <cstdlib>

struct A
{
  virtual void foo (void) {}
  virtual ~A () {}
};

struct B : public A
{
  virtual ~B () {}
};

struct C
{
  virtual ~C () {}
  int c;
};

struct D : public virtual B, public C
{
  virtual ~D () {}
};

struct E : public virtual D
{
  virtual ~E () {}
};

int
main ()
{
  char *v = new char[sizeof (E) + 16];
  memset (v, 0x55, sizeof (E) + 16);
  E *e = new (v) E ();
  e->~E ();

  for (unsigned i = sizeof (E); i < sizeof (E) + 16; ++i)
    if (v[i] != 0x55)
      abort ();

  delete[] v;
}
