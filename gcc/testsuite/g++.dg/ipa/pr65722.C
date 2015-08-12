// { dg-do compile }
// { dg-options "-O -fipa-icf -fno-rtti" }

struct A
{
  virtual void f ()
  {
    __builtin_abort ();
  }
  virtual void g ();
};

struct B : virtual A { };
struct C : B, virtual A { };

void foo()
{
  C c;
  C *p = &c;
  p->f ();
}
