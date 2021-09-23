/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */

typedef _Float16 v8hf __attribute__((vector_size (16)));
typedef _Float16 v16hf __attribute__((vector_size (32)));
typedef _Float16 v32hf __attribute__((vector_size (64)));

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

v8hf
vec_init_v8hf (_Float16 a1, _Float16 a2, _Float16 a3, _Float16 a4, _Float16 a5,
_Float16 a6, _Float16 a7, _Float16 a8)
{
    return __extension__ (v8hf) {a1, a2, a3, a4, a5, a6, a7, a8};
}

/* { dg-final { scan-assembler-times "vpunpcklwd" 4 } } */
/* { dg-final { scan-assembler-times "vpunpckldq" 2 } } */
/* { dg-final { scan-assembler-times "vpunpcklqdq" 1 } } */

VEC_EXTRACT (v8hf, _Float16, 4);
VEC_EXTRACT (v16hf, _Float16, 3);
VEC_EXTRACT (v16hf, _Float16, 8);
VEC_EXTRACT (v16hf, _Float16, 15);
VEC_EXTRACT (v32hf, _Float16, 5);
VEC_EXTRACT (v32hf, _Float16, 8);
VEC_EXTRACT (v32hf, _Float16, 14);
VEC_EXTRACT (v32hf, _Float16, 16);
VEC_EXTRACT (v32hf, _Float16, 24);
VEC_EXTRACT (v32hf, _Float16, 28);
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$8" 2 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$6" 1 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$14" 1 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$10" 1 } } */
/* { dg-final { scan-assembler-times "vpsrldq\[\t ]*\\\$12" 1 } } */
/* { dg-final { scan-assembler-times "vextract" 9 } } */

VEC_SET (v8hf, _Float16, 4);
VEC_SET (v16hf, _Float16, 3);
VEC_SET (v16hf, _Float16, 8);
VEC_SET (v16hf, _Float16, 15);
VEC_SET (v32hf, _Float16, 5);
VEC_SET (v32hf, _Float16, 8);
VEC_SET (v32hf, _Float16, 14);
VEC_SET (v32hf, _Float16, 16);
VEC_SET (v32hf, _Float16, 24);
VEC_SET (v32hf, _Float16, 28);
/* { dg-final { scan-assembler-times "vpbroadcastw" 10 } } */
/* { dg-final { scan-assembler-times "vpblendw" 4 } } */
/* { dg-final { scan-assembler-times "vpblendd" 3 } } */
