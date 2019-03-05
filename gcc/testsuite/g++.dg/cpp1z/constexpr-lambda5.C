// { dg-do compile { target c++17 } }

auto addOne = [] (int n) {
  return n + 1;
};
constexpr int (*addOneFp)(int) = addOne;
static_assert(addOneFp(3) == addOne(3), "");
