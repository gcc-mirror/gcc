// PR c++/82781
// { dg-do compile { target c++11 } }

typedef int V __attribute__ ((vector_size (16)));
constexpr V b1 = { 0, 1, 10, 20 };
constexpr V b2 = { 0, 2, 10, 0 };
constexpr V b3 = b1 == b2;

static_assert (b3[0] == -1, "");
static_assert (b3[1] == 0, "");
static_assert (b3[2] == -1, "");
static_assert (b3[3] == 0, "");
