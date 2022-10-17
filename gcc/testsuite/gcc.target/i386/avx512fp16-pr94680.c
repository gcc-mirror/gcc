/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vmovdqa" 4 } } */
/* { dg-final { scan-assembler-times "vmovq" 2 } } */

typedef _Float16 v32hf __attribute__((vector_size (64)));
typedef _Float16 v16hf __attribute__((vector_size (32)));
typedef _Float16 v8hf __attribute__((vector_size (16)));
typedef short v32hi __attribute__((vector_size (64)));
typedef short v16hi __attribute__((vector_size (32)));
typedef short v8hi __attribute__((vector_size (16)));


#define PERM_CONST_CONCAT0_v32hi \
{ 0, 1, 2, 3, 4, 5, 6, 7, \
  8, 9, 10, 11, 12, 13, 14, 15,	\
  34, 53, 41, 55, 57, 43, 36, 39, \
  62, 48, 50, 51, 49, 44, 60, 37 }

#define PERM_CONST_CONCAT0_v32hi_l \
{ 32, 33, 34, 35, 36, 37, 38, 39, \
  40, 41, 42, 43, 44, 45, 46, 47, \
  31, 0, 29, 2, 27, 4, 25, 6, 23, \
  8, 21, 10, 19, 12, 17, 14 }

#define PERM_CONST_CONCAT0_v16hi \
{ 0, 1, 2, 3, 4, 5, 6, 7, \
  21, 26, 17, 31, 24, 22, 30, 19 }

#define PERM_CONST_CONCAT0_v16hi_l \
{ 16, 17, 18, 19, 20, 21, 22, 23, \
  15, 0, 13, 2, 11, 4, 9, 6 }

#define PERM_CONST_CONCAT0_v8hi \
{ 0, 1, 2, 3, 9, 11, 14, 12 }

#define PERM_CONST_CONCAT0_v8hi_l \
{ 8, 9, 10, 11, 3, 5, 1, 7 }

#define PERM_CONST_CONCAT0(type) \
  PERM_CONST_CONCAT0_##type

#define PERM_CONST_CONCAT0_L(type) \
  PERM_CONST_CONCAT0_##type##_l

#define SHUFFLE_CONST_CONCAT0(type, itype) \
type foo_##type##shuffle_const_concat0 (type a) \
{ \
  return __builtin_shuffle (a, (type) {0}, \
			    (itype) PERM_CONST_CONCAT0 (itype)); \
} \
type foo_##type##shuffle_const_concat0_l (type a) \
{ \
  return __builtin_shuffle ((type) {0}, a, \
			    (itype) PERM_CONST_CONCAT0_L (itype)); \
}

SHUFFLE_CONST_CONCAT0 (v32hf, v32hi)
SHUFFLE_CONST_CONCAT0 (v16hf, v16hi)
SHUFFLE_CONST_CONCAT0 (v8hf, v8hi)

