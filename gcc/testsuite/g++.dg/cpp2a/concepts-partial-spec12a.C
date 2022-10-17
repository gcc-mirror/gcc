// PR c++/96363
// { dg-do compile { target c++20 } }
// A version of concepts-partial-spec12.C where the primary template is
// constrained.

template<class T> concept C = true;

template<C T> class TPL;

template<C T> requires true  class TPL<T>;   // #1
template<C T> requires false class TPL<T>;   // #2 error here

template<C T> requires true  class TPL<T*>;  // #1
template<C T> requires false class TPL<T*>;  // #2 error here
