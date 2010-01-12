// PR tree-optimization/42645
// { dg-do compile }
// { dg-options "-fcompare-debug -O1" }

struct C
{
  bool b;
  C ();
};

static inline C *foo () {}

extern void f4 ();

static inline int
f3 ()
{
  f4 ();
}

static inline void
f2 (bool b)
{
  int tmp = f3 ();
  if (C ().b && b)
    C ();
}

void
f1 ()
{
  C *c = foo ();
  f2 (c->b);
}
