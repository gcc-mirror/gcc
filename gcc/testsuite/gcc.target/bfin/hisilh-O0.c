/* { dg-do run { target bfin*-*-* } } */
/* { dg-options "-O0" } */
#include <stdlib.h>
typedef short raw2x16 __attribute__ ((vector_size(4)));

int x;

int ll(raw2x16 a, raw2x16 b)
{
    x = __builtin_bfin_mulhisill(a, b);
    return x;
}

int lh(raw2x16 a, raw2x16 b)
{
    x = __builtin_bfin_mulhisilh(a, b);
    return x;
}

int hl(raw2x16 a, raw2x16 b)
{
    x = __builtin_bfin_mulhisihl(a, b);
    return x;
}

int hh(raw2x16 a, raw2x16 b)
{
    x = __builtin_bfin_mulhisihh(a, b);
    return x;
}

int main ()
{
    raw2x16 a = __builtin_bfin_compose_2x16 (0x1234, 0x5678);
    raw2x16 b = __builtin_bfin_compose_2x16 (0xFEDC, 0xBA98);
    if (ll (a, b) != 0xe88e8740)
	abort ();
    if (lh (a, b) != 0xff9d5f20)
	abort ();
    if (hl (a, b) != 0xfb1096e0)
	abort ();
    if (hh (a, b) != 0xffeb3cb0)
	abort ();
    
    return 0;
}
