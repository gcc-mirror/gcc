// { dg-do compile }
// { dg-require-effective-target c++20 }
// { dg-options "-O2 -fdump-tree-evrp -fdump-tree-vrp1" }

namespace std
{
  constexpr bool isfinite (float x) { return __builtin_isfinite (x); }
  constexpr bool isfinite (double x) { return __builtin_isfinite (x); }
  constexpr bool isfinite (long double x) { return __builtin_isfinite (x); }
}

bool
foo (double x)
{
  if (!std::isfinite (x))
    __builtin_unreachable ();

  return std::isfinite (x);
}

bool
bar (double x)
{
  [[assume (std::isfinite (x))]];
  return std::isfinite (x);
}

/* { dg-final { scan-tree-dump "return 1;" "evrp" } } */
/* { dg-final { scan-tree-dump-times "return 1;" 2 "vrp1" } } */
