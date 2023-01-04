// PR c++/108206
// { dg-do compile { target c++11 } }

template <X x, typename T1> void foo (T1);	// { dg-error "'X' has not been declared" }
template <X x, typename T2> void foo (T2);	// { dg-error "'X' has not been declared" }
