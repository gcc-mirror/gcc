// P0634R3
// { dg-do compile { target c++20 } }

// Not in namespace scope.
template<typename T>
void fn1 ()
{
  // init-statement -> simple-declaration
  if (T::X r = 0; 0) // { dg-error "need .typename.|expected" }
    ;

  for (T::X g = 0; ;) // { dg-error "need .typename.|expected" }
    ;
}

template<typename T>
void
fn2 ()
{
  T::X fn3 (); // { dg-error "need .typename.|expected" }
  T::X v1; // { dg-error "need .typename.|expected" }
  T::X v2 = 0; // { dg-error "need .typename.|expected" }
  T::X v3{0}; // { dg-error "need .typename.|expected" }
  static constexpr T::X v4 = 0; // { dg-error "need .typename." }
  typedef T::X T2; // { dg-error "need .typename." }
}
