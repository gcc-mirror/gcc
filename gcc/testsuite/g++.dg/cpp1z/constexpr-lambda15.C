// PR c++/79461
// { dg-do compile { target c++14 } }

struct S {
  constexpr S(int i) {
    auto f = [i]{};		// { dg-error "10:variable .f. of non-literal type" "" { target c++14_only } }
  }
};
int main() {}

