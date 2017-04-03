// PR c++/79420

template<int> int f ()
{
  return f.x; // { dg-error "overloaded function with no contextual type information" }
}

void g ()
{
  f<0> ();
}
