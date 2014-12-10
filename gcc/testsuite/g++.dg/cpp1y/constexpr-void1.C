// { dg-do compile { target c++14 } }

struct S
{
  int i = 20;

  constexpr void
  foo (void)
  {
    if (i > 20)
      __builtin_abort ();
  }
};
