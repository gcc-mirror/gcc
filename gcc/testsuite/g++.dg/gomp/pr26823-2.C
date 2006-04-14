// PR middle-end/26823
// { dg-do compile }

struct A
{
  ~A () {}
};

extern void bar ();

void
foo ()
{
#pragma omp parallel
  {
    {
      A a;
      bar ();
    }
    {
      A a;
      bar ();
    }
    {
      A a;
      bar ();
    }
  }
}
