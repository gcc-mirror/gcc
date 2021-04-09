// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template <int I> int fn () 
{
  return I;
}

inline void g ()
{
  fn<1> (); // instantiation gets emitted
}
