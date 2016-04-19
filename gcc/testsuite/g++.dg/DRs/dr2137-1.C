// DR 2137
// { dg-do run { target c++11 } }

// Test that an initializer_list constructor beats the copy constructor.

#include <initializer_list>

bool ok = false;

struct Q {
  Q() = default;
  Q(Q const&) = default;
  Q(Q&&) = default;
  Q(std::initializer_list<Q>) { ok = true; }
};

int main() {
  Q x = Q { Q() };
  if (!ok) __builtin_abort ();
}
