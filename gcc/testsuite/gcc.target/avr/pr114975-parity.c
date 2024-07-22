/* { dg-do compile } */
/* { dg-additional-options "-Os" } */

typedef __UINT8_TYPE__ uint8_t;

uint8_t use_pary1 (int y, uint8_t x)
{
    return 1 + __builtin_parity (x);
}

uint8_t use_pary2 (uint8_t x)
{
	x += 1;
    return 1 - __builtin_parity (x);
}

/* { dg-final { scan-assembler-times "__parityqi2" 2 } } */
