// PR c++/104284
// { dg-do run { target c++14 } }
// { dg-additional-options "-fchecking=2" }

struct S {
  char c{};
};

template<class>
constexpr void g ()
{
  S s1[]{{}};
  if (s1[0].c != '\0')
    __builtin_abort ();
  S s2[]{{'a'}};
  if (s2[0].c != 'a')
    __builtin_abort ();
#if __cpp_designated_initializers >= 201707L
  S s3[]{{.c = 'a'}};
  if (s3[0].c != 'a')
    __builtin_abort ();
#endif
  S s4[]{'a'};
  if (s4[0].c != 'a')
    __builtin_abort ();
  S s5[]{{{}}};
  if (s5[0].c != '\0')
    __builtin_abort ();
}

int
main ()
{
  g<int>();
}
