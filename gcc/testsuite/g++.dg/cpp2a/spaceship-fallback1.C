// PR c++/100367
// { dg-do compile { target c++20 } }

#include <compare>

struct iter {
  bool current;
  iter(iter &);
};

constexpr bool operator==(const iter &, const iter &y) {
  return y.current;
}

void lexicographical_compare_three_way(iter a) {
  (a == a) <=> true;
}
