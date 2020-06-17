// PR c++/94583
// { dg-do compile { target c++20 } }

namespace std { struct strong_ordering { }; }

bool operator==(const struct Q&, const struct Q&);
struct Q {
  // { dg-error "defaulted after its first declaration" "" { target *-*-* } .+1 }
  friend std::strong_ordering operator<=>(const Q&, const Q&) = default;
};
bool b = Q() == Q();
