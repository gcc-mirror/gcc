// PR c++/29000
// { dg-options "" }

template<int> int foo()
{
  return ({foo;})==0;		// { dg-error "insufficient context" }
}
