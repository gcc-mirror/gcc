// PR gcov-profile/34609
// { dg-do compile }
// { dg-options "-O -ftest-coverage" }

struct A
{
  int i;
  int &get () { return i; }
};

inline A foo ()
{
  A a;
  a.get ();
  return a;
}

inline A bar ()
{
  return foo ();
}

void baz ()
{
  A a;
  a = bar ();
}

// { dg-final { cleanup-coverage-files } }
