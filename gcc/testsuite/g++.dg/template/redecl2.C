// { dg-do compile }

// Origin: heinlein@informatik.uni-ulm.de

// PR c++/14428: Redeclaration of class template with wrong
// non-type template parameter.

template <int i> struct X;	// { dg-error "template parameter" }
template <int* p> struct X;	// { dg-message "note: redeclared here" }
