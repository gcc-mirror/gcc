/* { dg-do compile } */
/* { dg-additional-options "-Os" } */

typedef __UINT8_TYPE__ uint8_t;

uint8_t use_pop1 (int y, uint8_t x)
{
    return 1 + __builtin_popcount (x);
}

uint8_t use_pop2 (uint8_t x)
{
	x += 1;
    return 1 - __builtin_popcount (x);
}

/* { dg-final { scan-assembler-times "__popcountqi2" 2 } } */
