/* { dg-do compile } */
/* { dg-require-effective-target c++11 } */
/* { dg-options "-O2 -msse4.1 -mfpmath=sse" } */

typedef float __attribute__( ( vector_size( 16 ) ) ) float32x4_t;

template<typename V1>
V1 vmax(V1 a, V1 b) {
    return (a>b) ? a : b;
}

template<typename V1>
V1 vmin(V1 a, V1 b) {
    return (a<b) ? a : b;
}

float foo(float a, float b, float c) {
    return vmin(vmax(a,b),c);
}

float32x4_t foo(float32x4_t a, float32x4_t b, float32x4_t c) {
    return vmin(vmax(a,b),c);
}

template<typename Float>
Float bart(Float a) { 
    constexpr Float zero{0.f};
    constexpr Float it = zero+4.f;
    constexpr Float zt = zero-3.f;
    return vmin(vmax(a,zt),it);
}

float bar(float a) {
    return bart(a);
}

float32x4_t bar(float32x4_t a) {
    return bart(a);
}

/* { dg-final { scan-assembler-times "min" 4 } } */
/* { dg-final { scan-assembler-times "max" 4 } } */
