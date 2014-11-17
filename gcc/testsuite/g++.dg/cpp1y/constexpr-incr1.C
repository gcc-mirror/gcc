// { dg-do compile { target c++14 } }

constexpr int f (int i)
{
  ++i;
  int x = i;
  ++x;
  return x;
}

constexpr int i = f(42);
#define SA(X) static_assert((X),#X)
SA(i==44);
