// PR c++/94583
// { dg-do compile { target c++2a } }

namespace std { struct strong_ordering { }; }

struct Q {
  friend std::strong_ordering operator<=>(const Q&, const Q&) = default;
};
bool operator==(const Q&, const Q&) noexcept;
