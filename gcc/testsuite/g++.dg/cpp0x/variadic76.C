// PR c++/33496
// { dg-do compile }
// { dg-options "-std=gnu++0x" }

template<int... N> int foo ()
{
  return sizeof... (N ());	// { dg-error "" }
  return sizeof... (N) ();	// { dg-error "" }
}

int bar ()
{
  return foo<0> ();
}
