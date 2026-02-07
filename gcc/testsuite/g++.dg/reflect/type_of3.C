// PR c++/123616
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>
using namespace std::meta;

struct S {
    auto g() { return 0; }
};
int h() { return 0; }

static_assert(type_of(^^S::g) == type_of(^^h));
