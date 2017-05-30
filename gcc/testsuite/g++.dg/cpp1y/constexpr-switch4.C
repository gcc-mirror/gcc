// PR c++/72868
// { dg-do compile }
// { dg-options "-std=gnu++14" }

constexpr int
foo (int i)
{
  switch (i)
    {
    case 11 ... 12:
      return 4;
    case 0 ... 9:
      return 3;
    default:
      return 7;
    }
}

#define SA(X) static_assert((X),#X)
SA (foo (-1) == 7);
SA (foo (0) == 3);
SA (foo (3) == 3);
SA (foo (9) == 3);
SA (foo (10) == 7);
SA (foo (11) == 4);
SA (foo (12) == 4);
SA (foo (13) == 7);
