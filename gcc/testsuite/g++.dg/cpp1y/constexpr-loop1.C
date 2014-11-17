// { dg-do compile { target c++14 } }

constexpr int f (int i)
{
  int j = 0;
  for (; i > 0; --i)
    ++j;
  return j;
}

constexpr int i = f(42);
#define SA(X) static_assert((X),#X)
SA(i==42);
