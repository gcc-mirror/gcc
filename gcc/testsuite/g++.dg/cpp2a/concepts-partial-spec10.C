// PR c++/92944
// { dg-do compile { target c++20 } }

namespace ns { template<class T> struct A { }; }

template<class T> requires true struct ns::A<T> { using type = T; };
template<class T> requires false struct ns::A<T> { };

template<class T> struct ns::A<T*> { };
template<class T> requires true struct ns::A<T*> { using type = T; };
template<class T> requires false struct ns::A<T*> { };

using ty1 = ns::A<int>::type;
using ty1 = int;

using ty2 = ns::A<int*>::type;
using ty2 = int;
