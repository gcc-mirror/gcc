// PR c++/79461
// { dg-do compile { target c++14 } }

struct S {
  constexpr S(int i) {
    auto f = [i]{};		// { dg-error "literal" "" { target c++14_only } }
  }
};
int main() {}

