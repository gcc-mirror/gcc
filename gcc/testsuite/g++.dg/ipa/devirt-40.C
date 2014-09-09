// PR c++/62224
// { dg-options "-O2" }
// For 4.9, we don't want to devirtualize f and thus create a reference to g.

struct A
{
  virtual void f () = 0;
};

class B : A
{
  virtual void f () { g(); }
  void g();
};

void h (A *a)
{
  a->f ();
}

// { dg-final { scan-assembler-not "_ZN1B1gEv" } }
