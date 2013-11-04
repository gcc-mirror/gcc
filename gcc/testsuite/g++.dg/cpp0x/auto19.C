// { dg-options -std=c++11 }

struct Explicit {
  Explicit() = default;  // Line 2
  explicit Explicit(const Explicit&){}
} ex;

auto ex2(ex); // Line 6
