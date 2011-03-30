// PR c++/48212
// { dg-options -std=c++0x }

template < bool > void
foo ()
{
  const bool b =;		// { dg-error "" }
  foo < b > ();			// { dg-error "constant expression" }
};
