// PR c++/112769
// { dg-do compile { target c++20 } }

template<int I, typename T>
struct type
{
    type(T) requires requires { T{0}; };
};

template <typename T>
using alias = type<0, T>;

alias foo{123};
