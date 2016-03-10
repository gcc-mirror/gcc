// PR c++/70135
// { dg-do run }
// { dg-options "-fsanitize=bounds -std=c++14" }

template <bool... b>
struct S {
  static constexpr bool c[] {b...};
  static constexpr auto foo ()
  {
    unsigned long n = 0;
    for (unsigned long i = 0; i < sizeof (c); i++)
      if (!c[i])
	++n;
    return n;
  }
  static constexpr auto n = foo () + 1;
  static constexpr auto bar ()
  {
    int h = 0;
    for (int g = 0, i = 0; g < n; ++g)
      {
	while (i < sizeof...(b) && c[i++])
	  ++h;
	h += 64;
      }
    return h;
  }
};

int
main ()
{
  S <true, false, false, true, true, true, false, true> s;
  constexpr auto c = s.bar ();
  static_assert (s.bar () == 4 * 64 + 5, "");
}
