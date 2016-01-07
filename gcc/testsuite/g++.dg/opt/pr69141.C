// PR tree-optimization/69141
// { dg-do compile }
// { dg-options "-O2 -fdump-tree-fre" }

struct B
{
  B *b;
  B ();
  virtual void f () = 0;
};

B::B () : b (this)
{
  b->f ();
}
