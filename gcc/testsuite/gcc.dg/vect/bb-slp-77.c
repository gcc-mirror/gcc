
/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
#include <stdint.h>
#include <string.h>


typedef struct {
    uint16_t d;
    uint16_t m;
    uint8_t val1[4];
    uint8_t val2[16];
} st1;

typedef struct {
    float d;
    float s;
    int8_t val2[32];
} st2;

float table[1 << 16];

inline static float foo(uint16_t f) {
    uint16_t s;
    memcpy(&s, &f, sizeof(uint16_t));
    return table[s];
}


void test(const int n, float * restrict s, const void * restrict vx, const void * restrict vy) {
    const int nb = n / 32;

    
    const st1 * restrict x = vx;
    const st2 * restrict y = vy;

    float sumf = 0.0;

    for (int i = 0; i < nb; i++) {
        uint32_t val1;
        memcpy(&val1, x[i].val1, sizeof(val1));

        int sumi0 = 0;
        int sumi1 = 0;

        if (val1) {
            for (int j = 0; j < 16; ++j) {
                const uint8_t xh_0 = ((val1 >> (j)) << 4) & 0x10;
                const uint8_t xh_1 = ((val1 >> (j + 12)) ) & 0x10;

                const int32_t x0 = (x[i].val2[j] & 0xF) | xh_0;
                const int32_t x1 = (x[i].val2[j] >> 4) | xh_1;

                sumi0 += (x0 * y[i].val2[j]);
                sumi1 += (x1 * y[i].val2[j + 16]);
            }
        } else {
            for (int j = 0; j < 16; ++j) {
                const int32_t x0 = (x[i].val2[j] & 0xF);
                const int32_t x1 = (x[i].val2[j] >> 4);

                sumi0 += (x0 * y[i].val2[j]);
                sumi1 += (x1 * y[i].val2[j + 16]);
            }
        }

        int sumi = sumi0 + sumi1;
        sumf += (foo(x[i].d)*y[i].d)*sumi + foo(x[i].m)*y[i].s;
    }

    *s = sumf;
}

/* { dg-final { scan-tree-dump-times "optimized: basic block" 1 "slp1"  { target { { vect_int_mult && vect_element_align } && { ! { powerpc*-*-* x86_64-*-* i?86-*-* } } } } } } */
