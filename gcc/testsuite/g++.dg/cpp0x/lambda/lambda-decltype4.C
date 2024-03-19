// PR c++/83167
// { dg-do compile { target c++11 } }

int main() {
  int x;
  const int y = 42;

  [] {
    using ty1 = decltype((x));
    using ty1 = int&;

    using ty2 = decltype((y));
    using ty2 = const int&;
  };
}
