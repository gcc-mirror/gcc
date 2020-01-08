// { dg-do compile { target c++11 } }

#if __cpp_consteval >= 201811L
consteval
#else
constexpr
#endif
int
foo (int x)
{
  return x * x * x * x;
}

auto a = foo (2);
