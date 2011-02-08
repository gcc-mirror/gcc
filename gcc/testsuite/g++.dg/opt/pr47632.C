// { dg-do compile }
// { dg-options "-O -fnon-call-exceptions -ftrapv" }

template < typename > struct S
{
  int n;
  void bar ()
    {
      int *i = new int[n];
    }
};

void
foo (S < char >*s)
{
  s->bar ();
}

