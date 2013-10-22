// { dg-options -std=c++11 }

struct IsLiteral {};

constexpr auto ab = IsLiteral();

constexpr IsLiteral bar(IsLiteral x) { return x; }

constexpr auto xy = bar(ab);
