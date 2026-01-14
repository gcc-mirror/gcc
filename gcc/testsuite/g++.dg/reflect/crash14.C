// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

constexpr int i = 0;
const int p = ++std::meta::extract<const int &>(^^i); // { dg-error "" }
