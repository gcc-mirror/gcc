#ifndef VEC_TYPES_H
#define VEC_TYPES_H 1

typedef __attribute__((vector_size(16))) signed char v16qi;
typedef __attribute__((vector_size(16))) unsigned char uv16qi;

typedef __attribute__((vector_size(16))) signed short v8hi;
typedef __attribute__((vector_size(16))) unsigned short uv8hi;

typedef __attribute__((vector_size(16))) signed int v4si;
typedef __attribute__((vector_size(16))) unsigned int uv4si;

typedef __attribute__((vector_size(16))) signed long long v2di;
typedef __attribute__((vector_size(16))) unsigned long long uv2di;

#if __SIZEOF_INT128__ == 16
typedef __attribute__((vector_size(16))) __int128_t v1ti;
#endif

typedef __attribute__((vector_size(16))) double v2df;
typedef __attribute__((vector_size(16))) long double v1tf;

#if __ARCH__ >= 12
typedef __attribute__((vector_size(16))) float v4sf;
#endif

#define GEN_SEQ_VEC(VEC_TYPE, ADDEND)					\
  ({ VEC_TYPE dummy;							\
    const int elts = sizeof(VEC_TYPE) / sizeof(dummy[0]);		\
    typeof(dummy[0]) __attribute__((aligned(8))) ar[elts];		\
    for (int i = 0; i < elts; i++)					\
      ar[i] = (typeof(dummy[0]))(i + (ADDEND));				\
    *(VEC_TYPE*)ar;})

#endif
