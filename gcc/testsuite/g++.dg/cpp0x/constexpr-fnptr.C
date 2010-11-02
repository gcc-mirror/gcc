// { dg-options -std=c++0x }

constexpr bool is_negative(int x) { return x < 0; }

constexpr bool check(int x, bool (*p)(int)) { return p(x); }  // #1

static_assert(check(-2, is_negative), "Error");
