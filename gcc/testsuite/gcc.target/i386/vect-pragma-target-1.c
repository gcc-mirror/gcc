/* { dg-do compile { target { i?86-*-* x86_64-*-* } } }                 */
/* { dg-options "-O0 -mno-sse3 -mtune=generic" }                        */
/* { dg-final { scan-assembler-times "paddd.+xmm\[0-9]+"        1 } }   */
/* { dg-final { scan-assembler-times "vfmadd132ps.+ymm\[0-9]+"  1 } }   */
/* { dg-final { scan-assembler-times "vpaddw.+zmm\[0-9]+"       1 } }   */
#ifndef CHECK_DEFINES
#define CHECK_DEFINES 0
#endif

#define N 1024

/* Optimization flags and tree vectorizer shall be disabled at this point */
#if CHECK_DEFINES && defined(__OPTIMIZE__)
#error "__OPTIMIZE__ is defined (not compiled with -O0?)"
#endif

#pragma GCC push_options
#pragma GCC optimize ("O2", "tree-vectorize")

/* Optimization flags and tree vectorizer shall be enabled at this point */
#if CHECK_DEFINES && !defined(__OPTIMIZE__)
#error "__OPTIMIZE__ is not defined"
#endif

#pragma GCC push_options
#pragma GCC target ("sse4.2")
#ifdef __cplusplus
namespace {
#endif

/* Target flags up to including SSE4.2 shall be enabled at this point */
#if CHECK_DEFINES && !defined(__SSE3__)
#error "Target flag (SSE3) is not defined"
#endif
#if CHECK_DEFINES && !defined(__SSSE3__)
#error "Target flag (SSSE3) is not defined"
#endif
#if CHECK_DEFINES && !defined(__SSE4_1__)
#error "Target flag (SSE4.1) is not defined"
#endif
#if CHECK_DEFINES && !defined(__SSE4_2__)
#error "Target flag (SSE4.2) is not defined"
#endif

void
__attribute__((__noinline__, __used__))
vec_saxpy_i32(int y[N], const int a[N], const int x[N])
{
    int i;
    for (i = 0; i < N; i++)
        y[i] += a[i] * x[i];
}

#ifdef __cplusplus
}
#endif
#pragma GCC pop_options

/* Target flags up to including SSE4.2 shall be disabled at this point */
#if CHECK_DEFINES && defined(__SSE3__)
#error "Target flag (SSE3) is still defined"
#endif
#if CHECK_DEFINES && defined(__SSSE3__)
#error "Target flag (SSSE3) is still defined"
#endif
#if CHECK_DEFINES && defined(__SSE4_1__)
#error "Target flag (SSE4.1) is still defined"
#endif
#if CHECK_DEFINES && defined(__SSE4_2__)
#error "Target flag (SSE4.2) is still defined"
#endif

#pragma GCC push_options
#pragma GCC target ("avx2", "fma")
#ifdef __cplusplus
struct A {
#endif

/* Target flags up to including AVX2+FMA shall be enabled at this point */
#if CHECK_DEFINES && !defined(__SSE3__)
#error "Target flag (SSE3) is not defined"
#endif
#if CHECK_DEFINES && !defined(__SSSE3__)
#error "Target flag (SSSE3) is not defined"
#endif
#if CHECK_DEFINES && !defined(__SSE4_1__)
#error "Target flag (SSE4.1) is not defined"
#endif
#if CHECK_DEFINES && !defined(__SSE4_2__)
#error "Target flag (SSE4.2) is not defined"
#endif
#if CHECK_DEFINES && !defined(__AVX__)
#error "Target flag (AVX) is not defined"
#endif
#if CHECK_DEFINES && !defined(__AVX2__)
#error "Target flag (AVX2) is not defined"
#endif
#if CHECK_DEFINES && !defined(__FMA__)
#error "Target flag (FMA) is not defined"
#endif

void
__attribute__((__noinline__, __used__))
vec_saxpy_f32(float y[N], const float a[N], const float x[N])
{
    int i;
    for (i = 0; i < N; i++)
        y[i] += a[i] * x[i];
}

#ifdef __cplusplus
};
#endif
#pragma GCC pop_options

/* Target flags up to including AVX2+FMA shall be disabled at this point */
#if CHECK_DEFINES && defined(__SSE3__)
#error "Target flag (SSE3) is still defined"
#endif
#if CHECK_DEFINES && defined(__SSSE3__)
#error "Target flag (SSSE3) is still defined"
#endif
#if CHECK_DEFINES && defined(__SSE4_1__)
#error "Target flag (SSE4.1) is still defined"
#endif
#if CHECK_DEFINES && defined(__SSE4_2__)
#error "Target flag (SSE4.2) is still defined"
#endif
#if CHECK_DEFINES && defined(__AVX__)
#error "Target flag (AVX) is still defined"
#endif
#if CHECK_DEFINES && defined(__AVX2__)
#error "Target flag (AVX2) is still defined"
#endif
#if CHECK_DEFINES && defined(__FMA__)
#error "Target flag (FMA) is still defined"
#endif

#pragma GCC push_options
#pragma GCC target ("arch=x86-64-v4")
#ifdef __cplusplus
namespace avx512 {
struct A {
#endif

/* Essential AVX512 target flags shall be enabled at this point */
#if CHECK_DEFINES && !defined(__AVX512F__)
#error "Target flag (AVX512F) is not defined"
#endif
#if CHECK_DEFINES && !defined(__AVX512VL__)
#error "Target flag (AVX512VL) is not defined"
#endif
#if CHECK_DEFINES && !defined(__AVX512DQ__)
#error "Target flag (AVX512DQ) is not defined"
#endif
#if CHECK_DEFINES && !defined(__AVX512BW__)
#error "Target flag (AVX512BW) is not defined"
#endif

void
__attribute__((__noinline__, __used__))
vec_saxpy_i16(short y[N], const short a[N], const short x[N])
{
    int i;
    for (i = 0; i < N; i++)
        y[i] += a[i] * x[i];
}

#ifdef __cplusplus
};
}
#endif
#pragma GCC pop_options

/* Essential AVX512 target flags shall be disabled at this point */
#if CHECK_DEFINES && defined(__AVX512F__)
#error "Target flag (AVX512F) is still defined"
#endif
#if CHECK_DEFINES && defined(__AVX512VL__)
#error "Target flag (AVX512VL) is still defined"
#endif
#if CHECK_DEFINES && defined(__AVX512DQ__)
#error "Target flag (AVX512DQ) is still defined"
#endif
#if CHECK_DEFINES && defined(__AVX512BW__)
#error "Target flag (AVX512BW) is still defined"
#endif

#pragma GCC pop_options

/* Optimization flags and tree vectorizer shall be disabled at this point */
#if CHECK_DEFINES && defined(__OPTIMIZE__)
#error "__OPTIMIZE__ is still defined"
#endif
