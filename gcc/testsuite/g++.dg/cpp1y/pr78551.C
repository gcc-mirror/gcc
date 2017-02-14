// { dg-do compile { target c++14 } }

// PR c++/78551 ICE in constexpr evaluation overwriting array
// intialized by string constant.

constexpr char Foo (char x, int ix)
{
  char d[4] = "012";
  d[0] = x;
  return d[ix];
}

static const char a = Foo ('a', 1);
static const char b = Foo ('a', 0);

static_assert (a == '1', "");
static_assert (b == 'a', "");

struct A {
  union {
    long s;
    char d[4];
  };
  constexpr A (char x)
    : d("012")
  { d[0] = x; }
};

static constexpr A c{'a'};

static_assert (c.d[0] == 'a', "");
static_assert (c.d[1] == '1', "");
