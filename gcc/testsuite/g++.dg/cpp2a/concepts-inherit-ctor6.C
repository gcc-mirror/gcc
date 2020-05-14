// PR c++/91930
// { dg-do compile { target c++20 } }

template <typename T> struct basic_mixin {
  basic_mixin() requires true;
};

struct mixin : basic_mixin<int> {
  using basic_mixin<int>::basic_mixin;
};

mixin m;
