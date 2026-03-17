// PR c++/124324
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

template<typename>
void function ();

constexpr auto substituted = substitute (^^function, {^^int});
static_assert (is_function (substituted));
constexpr auto t = type_of (substituted);
[: t :] bar;
