// PR c++/65646

template <typename = int> class A {};
template <> A<> &A<>::a;	// { dg-error "" }
