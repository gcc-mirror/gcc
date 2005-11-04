// PR c++/19989
// Don't instantiate a function template if it would generate an
//   array of size zero.

// { dg-do compile }

template<int T> struct cl {
  const static int value = T;
};

template<int I> void fn (char (*) [cl<I>::value] = 0 );

void foo (void)
{
  fn<0> ();  // { dg-error "no matching function" }
}

