// { dg-do compile { target c++14 } }
#define SA(X) static_assert((X),#X)

constexpr int f (int i)
{
  ++i;
  int x = i;
  ++x;
  return x;
}

constexpr int* g (int* p)
{
  ++p;
  return p;
}

constexpr int i = f(42);
SA(i==44);

int array[4];
constexpr int* p = g(array);
SA(p == &array[1]);
