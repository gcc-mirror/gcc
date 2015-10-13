// { dg-require-effective-target vect_simd_clones }
// { dg-additional-options "-fopenmp-simd -fno-inline -DONE_FILE" }
// { dg-additional-options "-mavx" { target avx_runtime } }

#include "../../gcc.dg/vect/tree-vect.h"

#ifdef ONE_FILE
#include "simd-clone-3.cc"
#else
#include "simd-clone-2.h"
#endif

T b __attribute__((aligned (32)));

void
do_main ()
{
  int i, r = 0;
  S a[64];
  for (i = 0; i < 64; i++)
    {
      a[i].s = i;
      b.t[i] = i;
    }
  #pragma omp simd reduction(+:r)
  for (i = 0; i < 64; i++)
    r += a[16].f0 (i);
  if (r != 64 * 63 / 2 + 64 * 16)
    __builtin_abort ();
  r = 0;
  #pragma omp simd reduction(+:r)
  for (i = 0; i < 64; i++)
    r += a[32].f1 (i);
  if (r != 64 * 63 / 2 + 64 * 32)
    __builtin_abort ();
  r = 0;
  #pragma omp simd reduction(+:r)
  for (i = 0; i < 64; i++)
    r += a[i].f2 (i);
  if (r != 64 * 63)
    __builtin_abort ();
  r = 0;
  #pragma omp simd reduction(+:r)
  for (i = 0; i < 64; i++)
    r += b.f3 (i);
  if (r != 64 * 63 / 2)
    __builtin_abort ();
}

int
main ()
{
  check_vect ();
  do_main ();
}
