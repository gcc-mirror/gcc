struct A;
typedef void (A::*Am1) (void *);
typedef void (A::*Am2) ();

struct B
{
  Am2 am2;
};

struct A
{
  A ();
  struct B b;
  struct C *c;
  struct D *d;
  void foo (Am1);
  void bar (void *);
};

struct C
{
};

