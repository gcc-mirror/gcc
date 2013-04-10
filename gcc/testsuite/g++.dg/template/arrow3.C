// PR c++/56895
// { dg-do compile }

extern struct A { bool foo (); A bar (); } *a;

template <int>
int
baz1 ()
{
  return 2 << (a->bar().foo() ? 1 : 0);
}

template <int>
int
baz2 ()
{
  return 2 >> (a->bar().foo() ? 1 : 0);
}

template <int>
int
baz3 ()
{
  return 10 / (a->bar().foo() ? 1 : 2);
}

template <int>
int
baz4 ()
{
  return 10 % (a->bar().foo() ? 1 : 0);
}

int
test ()
{
  return baz1<0> () + baz2<0> () + baz3<0> () + baz4<0> ();
}
