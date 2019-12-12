// Test for reversed candidates.
// { dg-do run { target c++2a } }

#include <compare>

struct D
{
  int i;
  auto operator<=>(int x) const { return i<=>x; }
  bool operator== (int x) const { return i==x;  }
};

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

template <class T>
void f()
{
  D d{42};
  int d1 = 42;
  int d2 = 24;

  assert (is_eq (d <=> d1));
  assert (is_eq (d1 <=> d));
  assert (is_lteq (d <=> d1));
  assert (is_lteq (d1 <=> d));
  assert (is_gteq (d <=> d1));
  assert (is_gteq (d1 <=> d));
  assert (is_lt (d2 <=> d));
  assert (is_lteq (d2 <=> d));
  assert (is_gt (d <=> d2));
  assert (is_gteq (d <=> d2));

  assert (d == d1);
  assert (d1 == d);
  assert (!(d2 == d));
  assert (!(d == d2));
  assert (d != d2);
  assert (d2 != d);
  assert (!(d != d1));
  assert (!(d1 != d));

  assert (d2 < d);
  assert (d2 <= d);
  assert (d1 <= d);
  assert (d > d2);
  assert (d >= d2);
  assert (d >= d1);
  assert (d <= d1);
}

int main()
{
  f<int>();
}
