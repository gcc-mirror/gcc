// { dg-options -std=c++14 }

auto l = []() constexpr { return 42; }; // { dg-error "constexpr" }

