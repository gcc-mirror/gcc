// PR c++/77771
// { dg-do compile { target c++11 } }
// { dg-options "-O" }

struct S
{
  char x[2];
  unsigned y;
};

constexpr bool func(const S s)
{
    return s.x[0] != 42 || s.x[1] != 0;
}

static_assert(func({{42, 7}, 0}), "");
