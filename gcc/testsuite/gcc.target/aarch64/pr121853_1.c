/* { dg-do run } */
/* { dg-additional-options "-O2 -std=c99" } */

#include <stdint.h>
#include <stdio.h>
#include <string.h>

__attribute__ ((noipa))
float convert(__bf16 value) {
    return (float)value;
}

static inline uint32_t f32_bits(float f) {
    uint32_t u; memcpy(&u, &f, sizeof u); return u;
}
static inline __bf16 bf16_from_bits(uint16_t u) {
    __bf16 b;  memcpy(&b, &u, sizeof b); return b;
}

/* Fixed bf16 inputs (as raw 16-bit payloads) covering edge cases.  */
static const uint16_t inputs[] = {
    0x0000, // +0
    0x8000, // -0
    0x7F80, // +inf
    0xFF80, // -inf
    0x7FC0, // qNaN (+)  (quiet bit set in bf16)
    0xFFC0, // qNaN (-)
    0x7F01, // sNaN (+)  (will be quieted by conversion)
    0xFF01, // sNaN (-)
    0x0001, // smallest +subnormal
    0x007F, // largest  +subnormal
    0x8001, // smallest -subnormal
    0x807F, // largest  -subnormal
    0x0080, // smallest +normal
    0x3F80, // +1.0
    0xBF80, // -1.0
    0x3F00, // +0.5
    0xBF00, // -0.5
    0x3FC0, // +1.5
    0x7F7F, // max finite +
    0xFF7F, // max finite -
};

int main(void) {
    const size_t N = sizeof(inputs)/sizeof(inputs[0]);
    size_t fails = 0;

    for (size_t i = 0; i < N; ++i) {
        __bf16 in = bf16_from_bits(inputs[i]);
        float out = convert(in);
        uint32_t got = f32_bits(out);
        uint32_t exp = inputs[i] << 16;

        if (got != exp) {
            printf("FAIL[%zu]: in_bf16=0x%04X  exp_f32=0x%08X  got_f32=0x%08X\n",
                   i, inputs[i], exp, got);
            ++fails;
        }
    }

    if (fails != 0)
      __builtin_abort ();
}

