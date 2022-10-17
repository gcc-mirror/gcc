/* { dg-do compile } */
/* { dg-options "-mf16c -msse2 -mno-avx2 -O2" } */

typedef __bf16 v8bf __attribute__ ((__vector_size__ (16)));
typedef __bf16 v16bf __attribute__ ((__vector_size__ (32)));

#define VEC_EXTRACT(V,S,IDX)			\
  S						\
  __attribute__((noipa))			\
  vec_extract_##V##_##IDX (V v)			\
  {						\
    return v[IDX];				\
  }

#define VEC_SET(V,S,IDX)			\
  V						\
  __attribute__((noipa))			\
  vec_set_##V##_##IDX (V v, S s)		\
  {						\
    v[IDX] = s;				\
    return v;					\
  }

v8bf
vec_init_v8bf (__bf16 a1, __bf16 a2, __bf16 a3, __bf16 a4,
	       __bf16 a5,  __bf16 a6, __bf16 a7, __bf16 a8)
{
    return __extension__ (v8bf) {a1, a2, a3, a4, a5, a6, a7, a8};
}

v16bf
vec_init_v16bf (__bf16 a1, __bf16 a2, __bf16 a3, __bf16 a4,
	       __bf16 a5,  __bf16 a6, __bf16 a7, __bf16 a8,
	       __bf16 a9,  __bf16 a10, __bf16 a11, __bf16 a12,
	       __bf16 a13,  __bf16 a14, __bf16 a15, __bf16 a16)
{
    return __extension__ (v16bf) {a1, a2, a3, a4, a5, a6, a7, a8,
				  a9, a10, a11, a12, a13, a14, a15, a16};
}

v8bf
vec_init_dup_v8bf (__bf16 a1)
{
    return __extension__ (v8bf) {a1, a1, a1, a1, a1, a1, a1, a1};
}

v16bf
vec_init_dup_v16bf (__bf16 a1)
{
    return __extension__ (v16bf) {a1, a1, a1, a1, a1, a1, a1, a1,
				  a1, a1, a1, a1, a1, a1, a1, a1};
}

/* { dg-final { scan-assembler-times "vpunpcklwd" 12 } } */
/* { dg-final { scan-assembler-times "vpunpckldq" 6 } } */
/* { dg-final { scan-assembler-times "vpunpcklqdq" 3 } } */

VEC_EXTRACT (v8bf, __bf16, 0);
VEC_EXTRACT (v8bf, __bf16, 4);
VEC_EXTRACT (v16bf, __bf16, 0);
VEC_EXTRACT (v16bf, __bf16, 3);
VEC_EXTRACT (v16bf, __bf16, 8);
VEC_EXTRACT (v16bf, __bf16, 15);
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$8" 1 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$6" 1 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$14" 1 } } */
/* { dg-final { scan-assembler-times "vextract" 4 } } */

VEC_SET (v8bf, __bf16, 4);
VEC_SET (v16bf, __bf16, 3);
VEC_SET (v16bf, __bf16, 8);
VEC_SET (v16bf, __bf16, 15);
/* { dg-final { scan-assembler-times "vpblendw" 3 { target { ! ia32 } } } } */

/* { dg-final { scan-assembler-times "vpinsrw" 30 { target ia32 } } } */

