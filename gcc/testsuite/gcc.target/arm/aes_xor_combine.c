/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-add-options arm_crypto } */
/* { dg-additional-options "-O3" } */

#include <arm_neon.h>

#define AESE(r, v, key) (r = vaeseq_u8 ((v), (key)));
#define AESD(r, v, key) (r = vaesdq_u8 ((v), (key)));

const uint8x16_t zero = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

uint8x16_t foo_key_0 (uint8x16_t dummy, uint8x16_t foo, uint8x16_t bar)
{
    dummy = dummy ^ foo;
    AESE(dummy, dummy, zero);
    dummy = dummy ^ bar;
    AESE(dummy, dummy, zero);

    dummy = dummy ^ foo;
    AESD(dummy, dummy, zero);
    dummy = dummy ^ bar;
    AESD(dummy, dummy, zero);

    return dummy;
}

uint8x16_t foo_data_0 (uint8x16_t dummy, uint8x16_t foo, uint8x16_t bar)
{
    dummy = dummy ^ foo;
    AESE(dummy, zero, dummy);
    dummy = dummy ^ bar;
    AESE(dummy, zero, dummy);

    dummy = dummy ^ foo;
    AESD(dummy, zero, dummy);
    dummy = dummy ^ bar;
    AESD(dummy, zero, dummy);

    return dummy;
}

/* { dg-final { scan-assembler-not "veor" } } */
