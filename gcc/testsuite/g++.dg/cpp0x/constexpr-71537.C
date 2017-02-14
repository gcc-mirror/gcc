// PR c++/71537
// { dg-do compile { target c++11 } }

constexpr int n[42] = {1};
constexpr int x1 = n ? 1 : 0;
constexpr int x2 = n + 1 ? 1 : 0;
constexpr int x3 = "abc" ? 1 : 0;
constexpr int x4 = "abc" + 1 ? 1 : 0;
constexpr bool x5 = "abc" + 1;
constexpr bool x6 = "abc" + 4;
constexpr bool x7 = n + 42;
static_assert (x1 == 1, "");
static_assert (x2 == 1, "");
static_assert (x3 == 1, "");
static_assert (x4 == 1, "");
static_assert (x5, "");
static_assert (x6, "");
static_assert (x7, "");
