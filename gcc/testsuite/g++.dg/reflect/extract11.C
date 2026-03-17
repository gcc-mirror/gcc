// PR c++/124324
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

consteval std::meta::info worst_sizeof_p1(std::meta::info ty) {
    static constexpr auto lam = []<class T>() static { return sizeof(T); };
    return substitute(^^decltype(lam)::template operator(), {ty});
}

constexpr std::meta::info ws = worst_sizeof_p1(^^int);
static_assert([: ws :]() == 4);
static_assert(is_function(ws));
static_assert(is_static_member(ws));

using F = size_t();
constexpr F* f = extract<F*>(ws);
