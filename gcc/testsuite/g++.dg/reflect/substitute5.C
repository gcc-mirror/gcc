// PR c++/124204
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

template<int>
using U = void;
constexpr int v = 0;
constexpr auto &ref = v;
constexpr auto result = substitute(^^U, {^^ref});
