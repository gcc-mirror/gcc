// PR c++/33496
// { dg-do compile }
// { dg-options "-std=gnu++11" }

template<int... N> int foo ()
{
  return sizeof... (N ());	// { dg-error "" }
  return sizeof... (N) ();	// { dg-error "" }
}

int bar ()
{
  return foo<0> ();
}
