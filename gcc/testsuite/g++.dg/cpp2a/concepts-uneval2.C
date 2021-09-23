// PR c++/99994
// { dg-do compile { target c++20 } }

int main() {
  auto f = [](int) { return true; };
  int n = [&](auto i) requires (f(sizeof(i))) { return 99; }(12);
  int m = [](auto i) requires (f(sizeof(i))) { return 99; }(12);
}
