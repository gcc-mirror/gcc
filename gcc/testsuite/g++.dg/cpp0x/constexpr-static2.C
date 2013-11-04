// { dg-options -std=c++11 }
struct IsLiteral {};

struct ShouldBeLiteral {
  constexpr ShouldBeLiteral(int){}
};

struct StaticDataMember {
  static constexpr IsLiteral one = IsLiteral(); // #1
  static constexpr ShouldBeLiteral two= ShouldBeLiteral(-1); // #2
};
