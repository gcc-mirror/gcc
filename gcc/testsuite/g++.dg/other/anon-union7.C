// PR c++/117153
// { dg-do compile { target c++11 } }

using U = union { union { int a; }; };

U
foo ()
{
  return {};
}

void
g ()
{
  [[maybe_unused]] auto u = foo ();
}
