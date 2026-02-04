// PR c++/123611
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template <typename T>
struct S {
  T s[2];
  constexpr const T *begin () const { return &s[0]; }
  constexpr const T *end () const { return &s[2]; }
};

unsigned
foo ()
{
  unsigned ret = 0;
  static constexpr S <decltype (^^int)> s = { ^^int, ^^bool };
  {
    static constexpr decltype (auto) r = (s);
    static constexpr auto b = r.begin ();
    static constexpr auto e = r.end ();
    using ptrdiff_t = decltype (&s - &s);
    constexpr auto N = [] consteval {
      ptrdiff_t res = 0;
      for (auto i = b; i != e; ++i) ++res;
      return res;
    };
    {
      static constexpr auto i = b + decltype (b - b) { ptrdiff_t (0) };
      constexpr auto x = *i;
      ret += sizeof (typename [: x :]);
    }
    {
      static constexpr auto i = b + decltype (b - b) { ptrdiff_t (1) };
      constexpr auto x = *i;
      ret += sizeof (typename [: x :]);
    }
  }
  return ret;
}

unsigned
bar ()
{
  unsigned ret = 0;
  static constexpr S <decltype (^^int)> s = { ^^int, ^^bool };
  template for (constexpr auto x : s)
    ret += sizeof (typename [: x :]);
  return ret;
}
