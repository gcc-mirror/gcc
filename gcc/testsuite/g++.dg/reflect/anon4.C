// PR c++/123642
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct foo
{
  int i;
  union
  {
    int a;
    long b;
    union
    {
      double c;
    };
  };
};

void test ()
{
  constexpr foo bar { .i = 11, .a = 1 };

  static_assert (bar.a == 1);
  static_assert (bar.[: ^^foo::a :] == 1); 

  static_assert (bar.*(&foo::a) == 1);
  static_assert (bar.*&[: ^^foo::a :] == 1); 

  constexpr foo bar1 { .i = 42, .c = 3.14 };

  static_assert (bar1.c == (double) 3.14);
  static_assert (bar1.[: ^^foo::c :] == (double) 3.14);
}
