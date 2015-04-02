// PR c++/65642
// { dg-do compile { target c++11 } }

// Check we're able to evaluate these.

#define SA(X) static_assert((X),#X)

constexpr char s[] = "abc";
constexpr int t[] = { 'a', 'b', 'c', '\0' };

constexpr char
fn1 (const char *p)
{
  return *(p + 1);
}

constexpr char
fn2 (const char *p)
{
  return p[1];
}

constexpr int
fn3 (const int *p)
{
  return *(p + 1);
}

constexpr int
fn4 (const int *p)
{
  return p[1];
}

constexpr auto c1 = fn1 (&s[0]);
constexpr auto c2 = fn1 (&s[1]);
constexpr auto c3 = fn1 (&s[2]);

SA (c1 == 'b');
SA (c2 == 'c');
SA (c3 == '\0');

constexpr auto d1 = fn2 (&s[0]);
constexpr auto d2 = fn2 (&s[1]);
constexpr auto d3 = fn2 (&s[2]);

SA (d1 == 'b');
SA (d2 == 'c');
SA (d3 == '\0');

constexpr auto e1 = fn3 (&t[0]);
constexpr auto e2 = fn3 (&t[1]);
constexpr auto e3 = fn3 (&t[2]);

SA (e1 == 'b');
SA (e2 == 'c');
SA (e3 == '\0');

constexpr auto f1 = fn4 (&t[0]);
constexpr auto f2 = fn4 (&t[1]);
constexpr auto f3 = fn4 (&t[2]);

SA (f1 == 'b');
SA (f2 == 'c');
SA (f3 == '\0');
