// PR target/88998
// { dg-do run { target sse2_runtime } }
// { dg-options "-O2 -msse2 -mfpmath=387" }
// { dg-require-effective-target c++11 }
// { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } }

#include <cassert>
#include <unordered_map>
#include <x86intrin.h>

double
__attribute__((noinline))
prepare (int a, int b)
{
  __m128i is = _mm_setr_epi32 (a, b, 0, 0);
  __m128d ds = _mm_cvtepi32_pd (is);
  return ds[0] + ds[1];
}

int
main (int, char **)
{
  double d = prepare (1, 2);

  std::unordered_map < int, int >m;
  m.insert ({0, 0});
  m.insert ({1, 1});
  assert (m.load_factor () <= m.max_load_factor ());

  assert (d == 3.0);
  return 0;
}
