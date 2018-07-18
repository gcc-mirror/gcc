// PR c++/82882
// { dg-do compile { target c++11 } }

template <typename>
void
foo ()
{
  auto v = [] { enum { E, F }; };
}

void
bar ()
{
  foo<int> ();
}
