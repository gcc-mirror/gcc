// PR ipa/64896
// { dg-do compile }
// { dg-options "-O2" }

struct A { int a, b; };
struct B { A c; int d; };
struct C { virtual B fn1 () const; };
struct D { B fn2 () const; void fn3 () const; C *fn4 () const; };

void
D::fn3 () const
{
  fn4 ()->fn1 ();
}

B
D::fn2 () const
{
  return B ();
}

class F : C
{
  B
  fn1 () const
  {
    return B ();
  }
};
