// { dg-do compile { target c++11 } }

template <class T> using Ptr = T*;
Ptr<unsigned>; // { dg-error "does not declare anything" }
Ptr<char><int>; // { dg-error "not a template|does not declare anything" }
template class Ptr<int>;//{ dg-error "alias template specialization\[^\n\r\]*after\[^\n\r\]*class" }

template <class T> using Arg = T;
struct A {};
template class Arg<A>;// { dg-error "alias templ\[^\n\r\]*specialization\[^\n\r\]*Arg<A>\[^\n\r\]*after\[^\n\r\]*class" }

template <template <class> class TT, class T> using Instantiate = TT<T>;
template <class> struct Vector {};

// The below is not OK, because of [dcl.type.elab]:
// 
//     If the identifier resolves to a typedef-name or the
//     simple-template-id resolves to an alias template
//     specialization, the elaborated-type-specifier is ill-formed.
//
template class Instantiate<Vector, int>;//{ dg-error "alias template specialization\[^\n\r\]*after\[^\n\r\]*class" }

template <class T> struct S {};
template<class T> using SFor = S<T>;
// Likewise, this is not OK.
template class SFor<int>; //{ dg-error "alias template specialization\[^\n\r\]*after\[^\n\r\]*class" }
