// PR c++/87603
// { dg-do compile { target c++11 } }

struct Y { };

          bool operator<(Y a, Y b) { return false; }
constexpr bool operator>(Y a, Y b) { return false; }

static_assert(!noexcept(Y{} > Y{}), "");
static_assert(!noexcept(Y{} < Y{}), "");
