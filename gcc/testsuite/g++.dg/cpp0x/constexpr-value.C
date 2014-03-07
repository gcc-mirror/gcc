// { dg-do compile { target c++11 } }

struct HopefullyLiteral {
  HopefullyLiteral() = default; // Should be a constexpr c'tor as of 12.1/6 and 8.4.2/4
};

constexpr HopefullyLiteral var1{}; // OK
constexpr HopefullyLiteral var2 = HopefullyLiteral{}; // #1
constexpr HopefullyLiteral var3 = HopefullyLiteral(); // #2
constexpr HopefullyLiteral var4 = HopefullyLiteral(var3); // #3
