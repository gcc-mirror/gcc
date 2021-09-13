// PR c++/98929
// { dg-do compile { target c++17 } }

template <typename T>
struct A {
  void foo ();
  using c = decltype (foo ());
  A (c);			// { dg-message {decltype \(A<T>::foo} }
};
A d;				// { dg-error "deduction failed" }
// { dg-error "no match" "" { target *-*-* } .-1 }
