// PR c++/117614
// { dg-do compile { target c++20 } }

constexpr int
foo ()
{
  union {
    int x{0};
    char y;
  };
  y = 1;
  return y;
}

constexpr int
bar ()
{
  union {
    union {
      int x{0};
      char y;
    };
    long long z;
  };
  y = 1;
  z = 2;
  return z;
}

static_assert (foo () == 1);
static_assert (bar () == 2);
