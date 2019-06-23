// PR c++/79655
// { dg-do compile { target c++14 } }

constexpr int
foo (int x, int y)
{
  int a[6] = { 1, 2, 3, 4, 5, 6 };
  a[x] = 0;			   // { dg-error "is outside the bounds" }
  return a[y];			   // { dg-error "is outside the bounds" }
}

constexpr int b = foo (0, -1);	// { dg-message "in .constexpr. expansion of " }
constexpr int c = foo (0, 6);	// { dg-message "in .constexpr. expansion of " }
constexpr int d = foo (6, 0);	// { dg-message "in .constexpr. expansion of " }
constexpr int e = foo (-1, 0);	// { dg-message "in .constexpr. expansion of " }
static_assert (foo (5, 5) == 0, "");
static_assert (foo (4, 5) == 6, "");
static_assert (foo (5, 4) == 5, "");
