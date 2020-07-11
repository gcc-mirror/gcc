// PR c++/94907
// { dg-do compile { target c++20 } }

namespace std { struct strong_ordering { }; }

struct E;
struct D {
  virtual std::strong_ordering operator<=>(const struct E&) const = 0;
};
struct E : D {
  std::strong_ordering operator<=>(const E&) const override = default;
};
