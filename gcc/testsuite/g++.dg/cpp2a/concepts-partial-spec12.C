// PR c++/96363
// { dg-do compile { target c++20 } }

template<class T> class TPL;

template<class T> requires true  class TPL<T>;   // #1
template<class T> requires false class TPL<T>;   // #2 error here

template<class T> requires true  class TPL<T*>;  // #1
template<class T> requires false class TPL<T*>;  // #2 error here
