// { dg-additional-options "-fmodules -std=c++20" }

struct exception_ptr {
  friend bool operator==(const exception_ptr&, const exception_ptr&) = default;
};

void enqueue() {
  exception_ptr e;
  e == e;
}

import "noexcept-4_a.H";

int main() {
  constexpr exception_ptr e;
  static_assert(e == e);
  static_assert(noexcept(e == e));
}
