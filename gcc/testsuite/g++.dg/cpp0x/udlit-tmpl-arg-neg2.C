// PR c++/77638
// { dg-do compile { target c++11 } }

template <T, T... U>		// { dg-error "'T' has not been declared" }
int operator"" _foo ();		// { dg-error "5:literal operator template .int operator\"\"_foo\\(\\). has invalid parameter list" }
template <T... U>		// { dg-error "'T' has not been declared" }
int operator"" _bar ();		// { dg-error "5:literal operator template .int operator\"\"_bar\\(\\). has invalid parameter list" }
