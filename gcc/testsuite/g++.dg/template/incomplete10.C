// PR c++/79420

struct S;
extern S s; // { dg-error "'s' has incomplete type" }
template<int> int f ()
{
  return s.x;
}

void g ()
{
  f<0> ();
}
