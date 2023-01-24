// PR ipa/108509
// { dg-do compile }
// { dg-options "-O1 -fdevirtualize -fno-tree-fre" }

struct B {
  virtual void deref ();
};

struct RefPtr {
  B *p;

  RefPtr ()
  {
    p->deref ();
  }
};

void
f ()
{
  RefPtr b;
}
