// PR c++/104284
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fchecking=2" }
// Like constexpr-104284.C, but the function template is not
// constexpr.  In that case, we were still calling build_vec_init
// in a template, just not crashing.

struct S {
  char c{};
};

template<class>
void g ()
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

void
f ()
{
  g<int>();
}
