/* PR target/119357 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */

#include <x86intrin.h>

typedef char V __attribute__((vector_size (16)));

void
foo (V *p)
{
  if (_mm_movemask_epi8 ((__m128i) (*p == 0)) != 65535)
    __builtin_abort ();
}
