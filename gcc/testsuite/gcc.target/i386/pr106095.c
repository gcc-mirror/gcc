/* PR gcc/106095 */
/* { dg-do assemble } */
/* { dg-options "-mavx512vl -masm=intel" } */
/* { dg-require-effective-target masm_intel } */

#include <immintrin.h>
#include <stdlib.h>

typedef int v4si __attribute__ ((vector_size (16)));
typedef long long v2di __attribute__ ((vector_size (16)));
typedef long long v4di __attribute__ ((vector_size (32)));
typedef double v2df __attribute__ ((vector_size (16)));
typedef float v4sf __attribute__ ((vector_size (16)));

void bug1(void) {
    v4si ints4 = {0, 1, 2, 3};
    unsigned long long *addr = malloc(sizeof(*addr));
    __builtin_ia32_pmovdw128mem_mask(addr, ints4, 0);
}

int bug2(void) {
    v2df a = {0.0, 0.0};
    v4sf b = {0.0, 0.0, 0.0, 0.0};
    v2df src = {0.0, 0.0};
    v2df res = __builtin_ia32_cvtss2sd_mask_round(a, b, src, 0, _MM_FROUND_NO_EXC);
    return (int)res[0];
}

int bug3(void) {
    v4sf a = {0.0, 0.0, 0.0, 0.0};
    v2df b = {0.0, 0.0};
    v4sf src = {0.0, 0.0, 0.0, 0.0};
    v4sf res = __builtin_ia32_cvtsd2ss_mask_round(a, b, src, 0, _MM_FROUND_NO_EXC);
    return (int)res[0];
}

int bug4(void) {
    v4di ints4 = {0, 1, 2, 3};
    unsigned long long *addr = malloc(sizeof(*addr));
    __builtin_ia32_pmovqw256mem_mask(addr, ints4, 0);
}

int bug5(void) {
    v2di ints4 = {0, 1};
    unsigned int *addr = malloc(sizeof(*addr));
    __builtin_ia32_pmovqw128mem_mask(addr, ints4, 0);
}
