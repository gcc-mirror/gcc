// { dg-do compile { target c++20 } }

struct A {
  char ar[10];
  constexpr A (const char *p) : ar()
  {
    for (int i = 0; i < 10; ++i)
      if ((ar[i] = p[i]) == 0)
	break;
  }
  // auto operator<=> (const A&) = default;
};

template <A a> constexpr A operator "" _sh() { return a; }

constexpr auto a = "foo"_sh;
static_assert (a.ar[0] == 'f');
