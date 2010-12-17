// { dg-options -std=c++0x }

struct Explicit {
  Explicit() = default;  // Line 2
  explicit Explicit(const Explicit&){}
} ex;

auto ex2(ex); // Line 6
