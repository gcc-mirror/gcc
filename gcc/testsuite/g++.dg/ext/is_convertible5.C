// PR c++/109277
// { dg-do compile { target c++11 } }
// { dg-options -fpermissive }

struct a;
struct b{};
static_assert (!__is_convertible (a, b), ""); // { dg-warning "incomplete" }
