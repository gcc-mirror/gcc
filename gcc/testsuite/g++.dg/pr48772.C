// { dg-do compile }
// { dg-options "-O -fnon-call-exceptions -fno-tree-ccp -fno-tree-dce" }

extern "C" void abort ();

struct A
{
  void foo ()
  {
    this->bar ();
  }
  virtual void bar ()
  {
    abort ();
  }
  ~A ()
  {
  }
};

struct B:A
{
  virtual void bar ()
  {
  }
};

int
main ()
{
  B b;
  b.foo ();
  return 0;
}

