// PR c++/65816
// { dg-do compile { target c++11 } }

struct test {
  int m;
  test() = default;
  constexpr test(int) : test() {}
};

static_assert(test(0).m == 0, "");
