// PR c++/56895
// { dg-do compile }

void fn (int *);
void fn (int);
extern struct A { bool foo (); A bar (); } *a;

template <int>
void
baz ()
{
  fn (a->bar().foo() ? 1 : 0);
}

void
test ()
{
  baz<0> ();
}
