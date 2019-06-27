// PR c++/90190
// { dg-do compile { target c++17 } }

#include <initializer_list>

enum class X {};

struct Term {
  double a;
  X i;
};

template <class It = const Term *>
struct sum {
  sum(std::initializer_list<Term>) {}
};

int main() {
  auto c2 = sum{{1, X()}, {2, X()}};
  auto c1 = sum{{1, X()}};  // fails only this
  auto c0 = sum{{}};
}
