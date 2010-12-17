// { dg-options -std=c++0x }

struct IsLiteral {};

constexpr auto ab = IsLiteral();

constexpr IsLiteral bar(IsLiteral x) { return x; }

constexpr auto xy = bar(ab);
