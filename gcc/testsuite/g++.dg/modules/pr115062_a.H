// PR c++/115062
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template <typename T> class S;
typedef S<char> X;
