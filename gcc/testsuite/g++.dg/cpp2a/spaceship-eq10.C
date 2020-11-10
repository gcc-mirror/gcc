// PR c++/95844
// { dg-do compile { target c++20 } }

#include <compare>

struct F {
  [[deprecated("oh no")]] std::strong_ordering operator<=>(const F&) const = default; // { dg-message "" }
};
void use_f(F f) {
  void(f == f);			// { dg-warning "deprecated" }
}
