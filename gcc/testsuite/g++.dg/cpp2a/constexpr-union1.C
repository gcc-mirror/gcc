// { dg-do compile { target c++2a } }

union U
{
  int x;
  char y;
};

constexpr bool
baz ()
{
  U u;
  u.x = 3;
  u.y = 7;
  return (u.y == 7);
}

static_assert (baz ());
