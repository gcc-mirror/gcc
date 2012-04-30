// PR c++/33496
// { dg-do compile }
// { dg-options "-std=gnu++0x" }

template<int... N> int foo ()
{
  return sizeof... (N ());	// { dg-error "cannot be used as a function" }
  return sizeof... (N) ();	// { dg-error "cannot be used as a function" }
}

int bar ()
{
  return foo<0> ();
}
