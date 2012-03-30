/* { dg-do run } */
/* { dg-options "-O2 -fpredictive-commoning -msse2 -std=c99" } */
/* { dg-require-effective-target sse2 } */

#include <x86intrin.h>

#include "isa-check.h"
#include "sse-os-support.h"

int main()
{
    const float mem[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };

    unsigned int indexes[8];
    for (unsigned int i = 0; i < 8; ++i) indexes[i] = i;

    check_isa ();

    if (!sse_os_support ())
      exit (0);

    __m128 x = _mm_setr_ps(0, 1, 2, 3);
    for (unsigned int i = 0; i + 4 < 6; ++i) {
        const unsigned int *ii = &indexes[i];
        const __m128 tmp = _mm_setr_ps(mem[ii[0]], mem[ii[1]], mem[ii[2]], mem[ii[3]]);
        if (0xf != _mm_movemask_ps(_mm_cmpeq_ps(tmp, x))) {
            __builtin_abort();
        }
        x = _mm_add_ps(x, _mm_set1_ps(1));
    }

    return 0;
}
