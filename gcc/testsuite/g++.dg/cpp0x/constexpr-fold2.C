// PR c++/65642
// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

constexpr char s[] = "abc";

constexpr bool
cmp (char const *a, char const *b)
{
  return a == b;
}

constexpr bool
fn1 (const char *s)
{
  return cmp (s, s + 1);
}

constexpr bool
fn2 (const char *s)
{
  return cmp (s + 1, s + 1);
}

constexpr auto c1 = fn1 (&s[0]);
constexpr auto c2 = fn2 (&s[0]);

SA (!c1);
SA (c2);
