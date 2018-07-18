// { dg-options -std=c++17 }

constexpr auto Add5 = [](int i) { return i+5; };

constexpr int x = Add5(4);
static_assert(x==9);
