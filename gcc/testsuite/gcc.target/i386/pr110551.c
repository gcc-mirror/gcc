/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

typedef unsigned long long uint64_t;

uint64_t mulx64(uint64_t x)
{
    __uint128_t r = (__uint128_t)x * 0x9E3779B97F4A7C15ull;
    return (uint64_t)r ^ (uint64_t)( r >> 64 );
}

/* { dg-final { scan-assembler-not "movq" } } */
