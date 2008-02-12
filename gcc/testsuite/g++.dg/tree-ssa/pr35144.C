// PR c++/35144
// { dg-do compile }
// { dg-options "-O2" }

struct A
{
  int baz ();
};

typedef int (A::*P) ();

struct B
{
  B ();
  int foo (P x, int y = 0);
};

struct C
{
  typedef int (B::*Q) (P, int);
  void bar (Q x) { c = x; }
  Q c;
};

extern C c;

B::B ()
{
 c.bar ((C::Q) &B::foo);
}
