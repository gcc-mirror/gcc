// PR c++/48212

template < bool > void
foo ()
{
  const bool b =;		// { dg-error "" }
  foo < b > ();			// { dg-error "" }
}
