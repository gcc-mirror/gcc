// { dg-options "-std=c++0x" }

template <class T> using Ptr = T*;
Ptr<unsigned>; // { dg-error "does not declare anything" }
Ptr<char><int>; // { dg-error "not a template|does not declare anything" }
template class Ptr<int>;//{ dg-error "explicit instantiation|non-class templ|does not decl|anything" }

template <class T> using Arg = T;
struct A {};
template class Arg<A>;// { dg-error "explicit instantiation|non-class templ" }

template <template <class> class TT, class T> using Instantiate = TT<T>;
template <class> struct Vector {};
template class Instantiate<Vector, int>; // OK Vector<int> can be explicitely instantiated

template <class T> struct S {};
template<class T> using SFor = S<T>;
template class SFor<int>; // OK, S<int> can be explicitely instantiated
