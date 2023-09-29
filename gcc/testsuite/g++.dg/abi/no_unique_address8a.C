// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=18 -Wabi=19" }

#include <cstddef>

#define NOUNIQUE [[no_unique_address]]

struct Empty { };
#define CHECK_DISTINCT(type, field1, field2) static_assert(offsetof(type, field1) != offsetof(type, field2))
#define CHECK_SAME(type, field1, field2) static_assert(offsetof(type, field1) == offsetof(type, field2))

struct A1 {
    NOUNIQUE Empty a;
    Empty b;
};
CHECK_DISTINCT(A1, a, b);
struct A2 {
    NOUNIQUE const Empty a;
    const Empty b;
};
CHECK_DISTINCT(A2, a, b);
struct A3 {			// { dg-warning "layout" }
    NOUNIQUE const Empty a;
    Empty b;
};
CHECK_SAME(A3, a, b);
struct A4 {			// { dg-warning "layout" }
    NOUNIQUE Empty a;
    const Empty b;
};
CHECK_SAME(A4, a, b);
