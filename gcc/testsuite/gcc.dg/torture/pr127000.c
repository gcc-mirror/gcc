/* { dg-do compile } */

#include <stdint.h>

typedef void *fp_t_2;
void *g8, *g11, *g19;
fp_t_2 f18f31_fp3;
void f18f31(uint64_t a0)
{
    g8 = 0;
    a0 = (char *)g11 - (char *)g8;
    *(uint64_t *)g19 = a0;
    f18f31_fp3 = *(fp_t_2 *)g19;
}
