// PR c++/71504
// { dg-do compile { target c++11 } }

typedef const char A4 [10];

constexpr A4 a [] = { "123", "123456", "123456789" };

constexpr int len (const char *s)
{
  return *s ? 1 + len (s + 1) : 0;
}

constexpr const char *s = a[0];
constexpr const char *t = (a + 2)[-2];

constexpr int n0 = len (s);
constexpr int n1 = len (t);

constexpr int n2 = len (a[0]);
constexpr int n3 = len ((a + 2)[-2]);

#define A(e) static_assert ((e), #e)

A (n0 == 3);
A (n0 == n1);
A (n0 == n2);
A (n0 == n3);
