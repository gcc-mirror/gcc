// PR c++/87372
// { dg-do compile { target c++14 } }

constexpr int
foo (char const *s)
{
  int i = 0;
  while (s[i])
    ++i;
  return i;
}

constexpr int
bar ()
{
  constexpr int l = foo (__PRETTY_FUNCTION__);
  constexpr int l2 = foo (__FUNCTION__);
  constexpr int l3 = foo (__func__);
  return l + l2 + l3;
}
static_assert (bar () == 25, "");
