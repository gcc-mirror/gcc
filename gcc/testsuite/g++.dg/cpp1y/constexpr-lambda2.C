// PR c++/126754
// { dg-do compile { target c++14 } }

struct S { int i; };
int main() {
  constexpr S a{1};
  const auto f = [](auto b) {
    return 1 - a.i;  // this expression must be independent of b in order to trigger the crash
  };
  return f(0);
}
