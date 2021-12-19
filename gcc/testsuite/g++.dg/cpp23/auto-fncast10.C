// { dg-do compile { target c++23 } }

struct S {
  int i1 : auto(12);
  int i2 : auto{12};
  static constexpr auto x = auto(12);
  static constexpr auto y = auto{12};
};

struct R {
  int i;
};

static constexpr R r1 = { auto(23) };
static constexpr R r2 = { auto{23} };
enum E { X = auto(12), Y = auto{1u} };
static_assert (auto(true));
static_assert (auto{true});
