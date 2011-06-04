/* { dg-do compile } */
/* { dg-options "-O2 -flto -fno-early-inlining -fkeep-inline-functions" } */
struct A
{
  virtual void foo () = 0;
};

struct B : A {};
struct C : A {};

struct D: C, B
{
  void foo () {}
};

static inline void
bar (B *b)
{
  b->foo ();
}

int
main ()
{
  D d;
  for (;;)
    bar (&d);
}
