// PR c++/70265
// { dg-do compile { target c++14 } }

constexpr int
foo (int p)
{
  int t = 0;
  while (1)  // { dg-error "count exceeds" }
    t = 0;
  return t;
}

static_assert (foo (1) == 0, "");  // { dg-error "non-constant|in .constexpr. expansion of " }
