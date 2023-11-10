// PR c++/79378
// { dg-do compile { target c++14 } }

int main() {
  int x = 0;
  [x=x, &r=x] {
    using ty1 = int;
    using ty1 = decltype(x);

    using ty2 = int&;
    using ty2 = decltype(r);
  };

  const int cx = 0;
  [x=cx, &r=cx] {
    using ty1 = int;
    using ty1 = decltype(x);

    using ty2 = const int&;
    using ty2 = decltype(r);
  };
}
