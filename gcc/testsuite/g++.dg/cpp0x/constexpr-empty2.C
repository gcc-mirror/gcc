// { dg-options -std=c++0x }

struct IsLiteral {};

constexpr IsLiteral bar(IsLiteral x) { return x; }

constexpr auto xy = bar(IsLiteral()); // #1  Error, but should be OK
