// PR c++/69261
// { dg-do run { target c++14 } }

typedef __SIZE_TYPE__ size_t;

template <size_t N>
struct S
{
  constexpr S() = default;

  template<size_t M>
  constexpr S (char const (&d)[M]) : data { 0 }
  {
    static_assert (M <= N, "size!");
    for (size_t i = 0; i != M; i++)
      data[i] = d[i];
  }
  char data[N];
};

template <int N>
constexpr S<N>
s (char const (&d)[N])
{
  S<N> c {};
  for (size_t i = 0; i != N; i++)
    c.data[i] = d[i];
  return c;
}

template <size_t N, size_t M>
constexpr auto
concat (S<N> const& s1, S<M> const& s2)
{
  S<N+M-1> s (s1.data);
  for (size_t i = 0; i != M; i++)
    s.data[N + i - 1] = s2.data[i];
  return s;
}

template <size_t N, size_t M>
constexpr auto
concat (char const (&x)[N], char const (&y)[M])
{
  S<N+M-1> tmp { x };
  for (size_t i = 0; i != M; i++)
    tmp.data[N+i-1] = y[i];
  return tmp;
}

int
main ()
{
  auto constexpr s1 = s ("bla");
  auto constexpr s2 = s ("blub");
  S<8> constexpr s1s2 = concat (s1, s2);
  auto constexpr c = concat ("bla", "blub");
  if (__builtin_strcmp (s1.data, "bla")
      || __builtin_strcmp (s2.data, "blub")
      || __builtin_strcmp (s1s2.data, "blablub")
      || __builtin_strcmp (c.data, "blablub"))
    __builtin_abort ();
}
