// { dg-options "-O2 -ffast-math" }
/* { dg-do run } */

float a = 1.f;
float b = 1.e20f;

float
fast()
{
  return __builtin_assoc_barrier (a + b) - b;
}

__attribute__((optimize("-fno-associative-math")))
float
normal()
{
  return a + b - b;
}

void test0()
{
  if (fast() != normal())
    __builtin_abort();
}

#ifdef __cplusplus
#ifdef __cpp_constexpr
constexpr float
pm(float x, float y)
{
  return __builtin_assoc_barrier(x + y) - y;
}

template <int x>
  constexpr int
  f()
  {
    return x;
  }
#endif

template <class T>
  T
  pm(T x, T y)
  {
    return __builtin_assoc_barrier(x + y) - y;
  }

void test1()
{
  if (pm(a, b) != normal())
    __builtin_abort();
#ifdef __cpp_constexpr
  constexpr float x = pm(1.f, 1.e20f);
  constexpr int y = f<int(pm(1.f, 1.e20f))>();
  if (x != normal())
    __builtin_abort();
  if (y != 0)
    __builtin_abort();
#endif
}
#else
void test1() {}
#endif

int main()
{
  test0();
  test1();
  return 0;
}
