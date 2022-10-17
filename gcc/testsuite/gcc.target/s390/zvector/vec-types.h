#ifndef VEC_TYPES_H
#define VEC_TYPES_H 1

#include <vecintrin.h>

typedef __vector signed char v16qi;
typedef __vector unsigned char uv16qi;

typedef __vector signed short v8hi;
typedef __vector unsigned short uv8hi;

typedef __vector signed int v4si;
typedef __vector unsigned int uv4si;

typedef __vector signed long long v2di;
typedef __vector unsigned long long uv2di;

#if __SIZEOF_INT128__ == 16
typedef __vector __int128_t v1ti;
#endif

typedef __vector double v2df;
typedef __vector long double v1tf;

#if __ARCH__ >= 12
typedef __vector float v4sf;
#endif

#define GEN_SEQ_VEC(VEC_TYPE, ADDEND)					\
  ({ VEC_TYPE dummy;							\
    const int elts = sizeof(VEC_TYPE) / sizeof(dummy[0]);		\
    typeof(dummy[0]) __attribute__((aligned(8))) ar[elts];		\
    for (int i = 0; i < elts; i++)					\
      ar[i] = (typeof(dummy[0]))(i + (ADDEND));				\
    *(VEC_TYPE*)ar;})

#endif
