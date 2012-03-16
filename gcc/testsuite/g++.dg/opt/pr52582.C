// PR c++/52582
// { dg-do compile }
// { dg-options "-O2" }

inline void *operator new (__SIZE_TYPE__, void *p) throw ()
{
  return p;
}

struct B
{
  virtual ~B ();
  B ();
};

struct A : B
{
  A () : B () {}
  virtual void bar ();
};

void
foo ()
{
  char a[64];
  B *b = new (&a) A ();
  b->~B ();
}
