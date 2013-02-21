/* { dg-do compile } */
/* { dg-options "-O -fipa-cp -std=gnu++0x -fno-early-inlining -fipa-cp-clone --param=ipa-cp-eval-threshold=1" } */

void bar (void *, void *);

struct C
{
  constexpr C ():p (0)
  {
  }
  void *get ()
  {
    return p;
  }
  void *p;
};

struct B:C
{
};

struct A
{
  void f (B * x, B * y)
  {
    bar (x->get (), y->get ());
  }
};

void
foo ()
{
  A a;
  B b;
  a.f (&b, &b);
}
