// { dg-options -std=c++1z }

constexpr auto Add5 = [](int i) { return i+5; };

constexpr int x = Add5(4);
static_assert(x==9);
