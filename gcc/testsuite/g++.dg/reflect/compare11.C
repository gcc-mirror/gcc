// PR c++/124331
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

template <typename T>
void f() {
    constexpr auto ctx = std::meta::access_context::current();
    static constexpr auto rng = std::define_static_array(nonstatic_data_members_of(^^T, ctx));
    static_assert(rng[0] == ^^T::mem);
};

struct S { int mem; };
auto force_instantiation = (f<S>, 0);
