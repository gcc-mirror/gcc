// PR c++/78523
// { dg-do compile { target c++14 } }

int bar ();

void
foo ()
{
  const int t = bar ();
  auto f = [=] (auto x) { return t; };
  f (0);
}
