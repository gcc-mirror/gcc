// PR c++/94190 - wrong no post-decrement operator error in template.

struct S { operator long & (); } b;

template<int> void
foo ()
{
  b--;
  ++b;
  --b;
  b++;
  !b;
  ~b;
  +b;
  -b;
}

void
bar ()
{
  foo<0> ();
}
