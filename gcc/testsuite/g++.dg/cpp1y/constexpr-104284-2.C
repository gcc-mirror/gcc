// PR c++/104284
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fchecking=2" }

struct S {
  char a;
  constexpr S() : a{'a'} { }
  constexpr S(char a_) : a{a_} { }
};

auto x1 = [](auto) { constexpr S s[]{{}}; };
auto x2 = [](auto) { constexpr S s[]{{'a'}}; };
auto x3 = [](auto) { constexpr S s[]{'a'}; };
auto x4 = [](auto) { constexpr S s[]{{{}}}; };

template<typename>
constexpr void g()
{
  constexpr S s1[]{{}};
  static_assert(s1[0].a == 'a', "");
  constexpr S s2[]{{'a'}};
  static_assert(s2[0].a == 'a', "");
  constexpr S s3[]{'a'};
  static_assert(s3[0].a == 'a', "");
  constexpr S s4[]{{{}}};
  static_assert(s4[0].a == '\0', "");
}

void
f ()
{
  g<int>();
}
