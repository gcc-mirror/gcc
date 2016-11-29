// PR c++/77638
// { dg-do compile { target c++11 } }

template <T, T... U>		// { dg-error "'T' has not been declared" }
int operator"" _foo ();		// { dg-error "has invalid parameter list" }
template <T... U>		// { dg-error "'T' has not been declared" }
int operator"" _bar ();		// { dg-error "has invalid parameter list" }
