// PR c++/25010
// { dg-options "-O2" }

#pragma interface

struct T
{
  T *p;

  void baz ()
  {
    p->baz ();
  }
};

void foo (T *p)
{
  p->baz ();
}

