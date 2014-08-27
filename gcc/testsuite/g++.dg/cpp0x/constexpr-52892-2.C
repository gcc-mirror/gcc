// PR c++/52892
// { dg-do compile { target c++11 } }

constexpr bool is_negative(int x) { return x < 0; }
typedef bool (*Function)(int);
constexpr bool check(int x, Function p) { return p(x); }
static_assert(check(-2, is_negative), "Error");
