// { dg-do compile { target c++14 } }

constexpr int f (int i)
{
  return 24;
  return 36;
}

constexpr int i = f(42);
#define SA(X) static_assert((X),#X)
SA(i==24);
