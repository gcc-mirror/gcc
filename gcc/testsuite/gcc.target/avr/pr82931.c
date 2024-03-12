/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-times "bst" 4 } } */
/* { dg-final { scan-assembler-times "bld" 4 } } */

typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;

#define BitMask (1u << 14)
#define Bit8Mask ((uint8_t) (1u << 4))

void merge1_8 (uint8_t *dst, const uint8_t *src)
{
    *dst = (*src & Bit8Mask) | (*dst & ~ Bit8Mask);
}

void merge2_8 (uint8_t *dst, const uint8_t *src)
{
    *dst ^= (*dst ^ *src) & Bit8Mask;
}

void merge1_16 (uint16_t *dst, const uint16_t *src)
{
    *dst = (*src & BitMask) | (*dst & ~ BitMask);
}

void merge2_16 (uint16_t *dst, const uint16_t *src)
{
    *dst ^= (*dst ^ *src) & BitMask;
}
