// PR c++/46626
// { dg-do run }
// { dg-options "-std=c++11" }

struct A
{
  virtual void f () = 0;
  virtual ~A () { }
};

struct B : A
{
  virtual void f () { }
};

static void
foo (A *a)
{
  a->f ();
}

int
main ()
{
  B b;
  foo (&b);
  return 0;
}
