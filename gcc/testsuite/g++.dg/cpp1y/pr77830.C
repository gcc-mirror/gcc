// PR c++/77830
// { dg-do compile { target c++14 } }

template <int N>
struct P
{
  char arr[N][1];
  constexpr void foo (const char *, int);
};

template <int N>
constexpr void
P<N>::foo (const char *, int i)
{
  for (auto j = 0; j < 2; ++j)
    arr[i][j] = true;
}

template <typename... T>
constexpr auto
bar (T... a)
{
  const char *s[]{a...};
  P<sizeof...(a)> p{};
  for (auto i = 0; i < sizeof...(a); ++i)
    p.foo (s[i], i); // { dg-message "in .constexpr. expansion of " }
  return p;
}

int
main ()
{
  constexpr auto a = bar ("", "");	// { dg-error "outside the bounds of array type|in .constexpr. expansion of " }
}
