// PR c++/96917
// { dg-do compile { target c++14 } }

int main() {
  int x = 0;
  int y = 0;
  const int cx = 0;
  const int cy = 0;

  [x, &y, cx, &cy] {
    decltype(auto) a = x;
    using ty1 = int;
    using ty1 = decltype(x);
    using ty1 = decltype(a);

    decltype(auto) b = y;
    using ty2 = int;
    using ty2 = decltype(y);
    using ty2 = decltype(b);

    decltype(auto) ca = cx;
    using ty3 = const int;
    using ty3 = decltype(cx);
    using ty3 = decltype(ca);

    decltype(auto) cb = cy;
    using ty4 = const int;
    using ty4 = decltype(cy);
    using ty4 = decltype(cb);
  };

  [x=x, &y=y, cx=cx, &cy=cy] {
    decltype(auto) a = x;
    using ty1 = int;
    using ty1 = decltype(x);
    using ty1 = decltype(a);

    decltype(auto) b = y;
    using ty2 = int&;
    using ty2 = decltype(y);
    using ty2 = decltype(b);

    decltype(auto) ca = cx;
    using ty3 = int;
    using ty3 = decltype(cx);
    using ty3 = decltype(ca);

    decltype(auto) cb = cy;
    using ty4 = const int&;
    using ty4 = decltype(cy);
    using ty4 = decltype(cb);
  };
}
