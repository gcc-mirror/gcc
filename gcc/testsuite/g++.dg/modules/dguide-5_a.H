// PR c++/117397
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

template <typename T> struct S;
template <typename T> S(T) -> S<T>;
