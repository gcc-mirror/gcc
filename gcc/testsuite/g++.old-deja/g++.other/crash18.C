// { dg-do assemble  }
// { dg-options "-fvtable-gc -S" }
// Origin: Mark Mitchell <mitchell@codesourcery.com>

struct S {
  virtual void f ();
};

S* s;

void g ()
{
  s->f ();
}
