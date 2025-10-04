// C++ 26 P2795R5 - Erroneous behaviour for uninitialized reads
// { dg-do compile { target c++23 } }
// Make sure we don't reject this in C++26 because of
// .DEFERRED_INIT calls.

constexpr int
foo (int x)
{
  if (x == 6)
    goto l1;
  if (x == 7)
    goto l2;
  int i;
  switch (x)
    {
      int j;
    case 1:
      i = 6;
      return i;
    case 2:
      i = 4;
    l1:
      i = 5;
      return i;
    case 3:
    l2:
      i = 7;
      return i;
    default:
      return 42;
    }
}

static_assert (foo (1) == 6);
static_assert (foo (2) == 5);
static_assert (foo (3) == 7);
static_assert (foo (4) == 42);
