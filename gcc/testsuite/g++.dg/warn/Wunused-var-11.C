// PR c++/44412
// { dg-do compile }
// { dg-options "-Wunused" }

struct S
{
  int foo ();
  static int bar ();
};

int S::foo ()
{
  return 5;
}

int S::bar ()
{
  return 6;
}

int
f1 ()
{
  S s;
  return s.foo ();
}

int
f2 ()
{
  S s;
  return s.bar ();
}
