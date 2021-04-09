// { dg-do compile { target c++20 } }

#include <cmath>

template<auto> class MyClass { };

static_assert(__is_same(MyClass<NAN>, MyClass<NAN>));

constexpr auto mynan = NAN;
static_assert(__is_same(MyClass<mynan>, MyClass<mynan>));

static_assert(__is_same(MyClass<NAN>, MyClass<mynan>));
