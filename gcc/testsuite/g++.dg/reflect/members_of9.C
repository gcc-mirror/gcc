// PR c++/124215
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct Test {
    int a;
    char b;
};

consteval auto number_of_members(std::meta::info ty) -> size_t {
    return nonstatic_data_members_of(ty, std::meta::access_context::current()).size();
}

static_assert(number_of_members(^^Test) == 2);
static_assert(number_of_members(^^Test const) == 2);
