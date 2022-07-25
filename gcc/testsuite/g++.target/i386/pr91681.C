/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

void multiply128x64x2_3 ( 
    const unsigned long a, 
    const unsigned long b, 
    const unsigned long c, 
    const unsigned long d, 
    __uint128_t o[2])
{
    __uint128_t B0 = (__uint128_t) b * c;
    __uint128_t B2 = (__uint128_t) a * c;
    __uint128_t B1 = (__uint128_t) b * d;
    __uint128_t B3 = (__uint128_t) a * d;

    o[0] = B2 + (B0 >> 64);
    o[1] = B3 + (B1 >> 64);
}

/* { dg-final { scan-assembler-not "xor" } } */
