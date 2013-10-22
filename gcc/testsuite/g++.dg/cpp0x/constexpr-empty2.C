// { dg-options -std=c++11 }

struct IsLiteral {};

constexpr IsLiteral bar(IsLiteral x) { return x; }

constexpr auto xy = bar(IsLiteral()); // #1  Error, but should be OK
