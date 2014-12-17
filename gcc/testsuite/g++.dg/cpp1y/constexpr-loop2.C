// { dg-do compile { target c++14 } }

constexpr int f(int* p) { return *p; }
constexpr int g(int n)
{
  int sum = 0;
  for (int i = 1; i <= n; ++i)
    sum += f(&i);
  return sum;
}

static_assert(g(3) == 3+2+1,"");
