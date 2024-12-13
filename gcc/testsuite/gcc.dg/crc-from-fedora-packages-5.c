/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// crc32.i
// We don't support this case.

#include <stddef.h>
typedef unsigned int u32;
u32 gf2_multiply(u32 x, u32 y, u32 modulus)
{
    u32 product = x & 1 ? y : 0;
    int i;

    for (i = 0; i < 31; i++) {
        product = (product >> 1) ^ (product & 1 ? modulus : 0);
        x >>= 1;
        product ^= x & 1 ? y : 0;
    }

    return product;
}

u32 crc32_generic_shift(u32 crc, size_t len,
                        u32 polynomial)
{
    u32 power = 0x2101;
    int i;

    for (i = 0; i < 8 * (int)(len & 3); i++)
        crc = (crc >> 1) ^ (crc & 1 ? 0x2101 : 0);

    len >>= 2;
    if (!len)
        return crc;

    for (;;) {

        if (len & 1)
            crc = gf2_multiply(crc, power, polynomial);

        len >>= 1;
        if (!len)
            break;


        power = gf2_multiply(power, power, polynomial);
    }

    return crc;
}

/* { dg-final { scan-tree-dump "Loop iteration number isn't a constant." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 30" "crc" } } */
