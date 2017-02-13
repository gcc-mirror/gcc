// PR c++/79461
// { dg-options -std=c++1z }

struct S {
  constexpr S(int i) {
    auto f = [i]{};
  }
};
int main() {}

