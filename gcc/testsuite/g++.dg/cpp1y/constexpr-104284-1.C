// PR c++/104284
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fchecking=2" }

struct S {
  char c{};
};

auto x1 = [](auto) { constexpr S s[]{{}}; };
auto x2 = [](auto) { constexpr S s[]{{'a'}}; };
#if __cpp_designated_initializers >= 201707L
auto x3 = [](auto) { constexpr S s[]{{.c = 'a'}}; };
#endif
auto x4 = [](auto) { constexpr S s[]{'a'}; };
auto x5 = [](auto) { constexpr S s[]{{{}}}; };

template<class>
constexpr void g ()
{
  constexpr S s1[]{{}};
  static_assert(s1[0].c == '\0', "");
  constexpr S s2[]{{'a'}};
  static_assert(s2[0].c == 'a', "");
#if __cpp_designated_initializers >= 201707L
  constexpr S s3[]{{.c = 'a'}};
  static_assert(s3[0].c == 'a', "");
#endif
  constexpr S s4[]{'a'};
  static_assert(s4[0].c == 'a', "");
  constexpr S s5[]{{{}}};
  static_assert(s5[0].c == '\0', "");
}

static_assert ((g<int>(), true), "");
