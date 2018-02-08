// PR c++/70590
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }

int a;

constexpr int *foo = &a;

void blah (int *);

void
bar ()
{
  blah (foo);
}

void
baz ()
{
  blah (foo);
}
