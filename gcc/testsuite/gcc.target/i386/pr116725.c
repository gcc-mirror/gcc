/* PR gcc/116725 */
/* { dg-do assemble } */
/* { dg-options "-masm=intel -mavx512dq -mavx512fp16 -mavx512vl" } */
/* { dg-require-effective-target masm_intel } */
/* { dg-require-effective-target avx512dq } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-require-effective-target avx512vl } */

#include <stdio.h>

typedef double __m128d __attribute__ ((__vector_size__ (16)));
typedef float __m128f __attribute__ ((__vector_size__ (16)));
typedef int __v16si __attribute__ ((__vector_size__ (64)));
typedef _Float16 __m256h __attribute__ ((__vector_size__ (32)));
typedef long long __m512i __attribute__((__vector_size__(64)));
typedef _Float16 __m128h __attribute__ ((__vector_size__ (16), __may_alias__));
typedef int __v4si __attribute__ ((__vector_size__ (16)));
typedef long long __m128i __attribute__ ((__vector_size__ (16)));

int main(void) {
    __m128d vec = {1.0, 2.0};
    char res = __builtin_ia32_fpclasssd_mask(vec, 1, 1);
    printf("%d\n", res);

    __m128f vec2 = {1.0, 2.0, 3.0, 4.0};
    char res2 = __builtin_ia32_fpclassss_mask(vec2, 1, 1);
    printf("%d\n", res2);

    __m128h vec3 = {2.0, 1.0, 3.0};
    __v4si vec4 = {};
    __v4si res3 = __builtin_ia32_vcvtph2dq128_mask(vec3, vec4, -1);
    printf("%d\n", res3[0]);

    __v4si res4 = __builtin_ia32_vcvtph2udq128_mask(vec3, vec4, -1);
    printf("%d\n", res4[0]);

    __m128i vec5 = {};
    __m128i res5 = __builtin_ia32_vcvtph2qq128_mask(vec3, vec5, -1);
    printf("%d\n", res5[0]);

    __m128i res6 = __builtin_ia32_vcvtph2uqq128_mask(vec3, vec5, -1);
    printf("%d\n", res6[0]);
}
