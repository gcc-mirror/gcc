// { dg-do run { target { ! ia32 } } }
// { dg-require-effective-target c++11 }
// { dg-options "-O2 -march=k8" }

using float2[[gnu::vector_size (8)]] = float;
using int2[[gnu::vector_size (8)]] = int;
float2 y = { 2, 2 };

int
main ()
{
  const auto k = y == float2 { 2, 2 };
  if (k[1] == 0)
    __builtin_abort ();
  const auto a = k & int2 { 2, 2 };
  return a[0] - 2;
}
