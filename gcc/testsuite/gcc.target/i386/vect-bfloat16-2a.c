/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */

typedef __bf16 v8bf __attribute__ ((__vector_size__ (16)));
typedef __bf16 v16bf __attribute__ ((__vector_size__ (32)));
typedef __bf16 v32bf __attribute__ ((__vector_size__ (64)));

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

v32bf
vec_init_v32bf (__bf16 a1, __bf16 a2, __bf16 a3, __bf16 a4,
		__bf16 a5, __bf16 a6, __bf16 a7, __bf16 a8,
		__bf16 a9, __bf16 a10, __bf16 a11, __bf16 a12,
		__bf16 a13, __bf16 a14, __bf16 a15, __bf16 a16,
		__bf16 a17, __bf16 a18, __bf16 a19, __bf16 a20,
		__bf16 a21, __bf16 a22, __bf16 a23, __bf16 a24,
		__bf16 a25, __bf16 a26, __bf16 a27, __bf16 a28,
		__bf16 a29, __bf16 a30, __bf16 a31, __bf16 a32)
{
    return __extension__ (v32bf) {a1, a2, a3, a4, a5, a6, a7, a8,
				  a9, a10, a11, a12, a13, a14, a15, a16,
				  a17, a18, a19, a20, a21, a22, a23, a24,
				  a25, a26, a27, a28, a29, a30, a31, a32};
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

v32bf
vec_init_dup_v32bf (__bf16 a1)
{
    return __extension__ (v32bf) {a1, a1, a1, a1, a1, a1, a1, a1,
				  a1, a1, a1, a1, a1, a1, a1, a1,
				  a1, a1, a1, a1, a1, a1, a1, a1,
				  a1, a1, a1, a1, a1, a1, a1, a1};
}

/* { dg-final { scan-assembler-times "vpunpcklwd" 28 } } */
/* { dg-final { scan-assembler-times "vpunpckldq" 14 } } */
/* { dg-final { scan-assembler-times "vpunpcklqdq" 7 } } */

VEC_EXTRACT (v8bf, __bf16, 0);
VEC_EXTRACT (v8bf, __bf16, 4);
VEC_EXTRACT (v16bf, __bf16, 0);
VEC_EXTRACT (v16bf, __bf16, 3);
VEC_EXTRACT (v16bf, __bf16, 8);
VEC_EXTRACT (v16bf, __bf16, 15);
VEC_EXTRACT (v32bf, __bf16, 0);
VEC_EXTRACT (v32bf, __bf16, 5);
VEC_EXTRACT (v32bf, __bf16, 8);
VEC_EXTRACT (v32bf, __bf16, 14);
VEC_EXTRACT (v32bf, __bf16, 16);
VEC_EXTRACT (v32bf, __bf16, 24);
VEC_EXTRACT (v32bf, __bf16, 28);
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$8" 2 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$6" 1 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$14" 1 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$10" 1 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$12" 1 } } */
/* { dg-final { scan-assembler-times "vextract" 9 } } */

VEC_SET (v8bf, __bf16, 4);
VEC_SET (v16bf, __bf16, 3);
VEC_SET (v16bf, __bf16, 8);
VEC_SET (v16bf, __bf16, 15);
VEC_SET (v32bf, __bf16, 5);
VEC_SET (v32bf, __bf16, 8);
VEC_SET (v32bf, __bf16, 14);
VEC_SET (v32bf, __bf16, 16);
VEC_SET (v32bf, __bf16, 24);
VEC_SET (v32bf, __bf16, 28);
/* { dg-final { scan-assembler-times "vpbroadcastw" 13 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpblendw" 4 { target { ! ia32 } } } } */

/* { dg-final { scan-assembler-times "vpbroadcastw" 12 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vpblendw" 3 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vpinsrw" 1 { target ia32 } } } */

/* { dg-final { scan-assembler-times "vpblendd" 3 } } */
