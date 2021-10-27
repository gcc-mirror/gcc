/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-not "movw" } } */
/* { dg-final { scan-assembler-times "vpermi2w" 3 } } */
/* { dg-final { scan-assembler-times "vpermw" 6 } } */
/* { dg-final { scan-assembler-times "vpshufb" 3 } } */
/* { dg-final { scan-assembler-times "vpermt2w" 6 } } */

typedef _Float16 v32hf __attribute__((vector_size (64)));
typedef _Float16 v16hf __attribute__((vector_size (32)));
typedef _Float16 v8hf __attribute__((vector_size (16)));
typedef short v32hi __attribute__((vector_size (64)));
typedef short v16hi __attribute__((vector_size (32)));
typedef short v8hi __attribute__((vector_size (16)));

#define PERM_CONST_RANDOM_v32hi	\
{ 0, 21, 15, 9, 43, 25, 37, 48,	\
  8, 16, 27, 51, 30, 12, 6, 46,	\
  34, 3, 11, 5, 17, 53, 26, 39,	\
  2, 18, 40, 61, 19, 4, 50, 29 }

#define PERM_CONST_RANDOM_RANGE32_v32hi \
{ 0, 21, 10, 23, 8, 18, 7, 19, \
  4, 25, 3, 31, 5, 22, 11, 17, \
  9, 20, 2, 24, 1, 30, 12, 27, \
  13, 28, 6, 29, 14, 16, 15, 23 }

#define PERM_CONST_RANDOM_v16hi \
{ 0, 21, 15, 9, 13, 25, 30, 18,	\
  8, 16, 17, 11, 4, 22, 6, 7 }

#define PERM_CONST_RANDOM_RANGE16_v16hi \
{ 0, 9, 1, 12, 4, 15, 7, 13,	\
  3, 10, 6, 14, 5, 8, 2, 11 }

#define PERM_CONST_RANDOM_v8hi \
{ 0, 14, 15, 9, 13, 2, 3, 5 }

#define PERM_CONST_RANDOM_RANGE8_v8hi \
{ 0, 7, 2, 5, 3, 4, 1, 6 }

#define PERM_CONST_RANDOM(size)	\
  PERM_CONST_RANDOM_v##size##hi

#define PERM_CONST_RANDOM_RANGE(size) \
  PERM_CONST_RANDOM_RANGE##size##_v##size##hi

#define SHUFFLE_CONST_RANDOM(type, itype, size) \
type foo_##type##shuffle_2param_const_random (type a, type b) \
{ \
  return __builtin_shuffle (a, b, \
			    (itype) PERM_CONST_RANDOM (size)); \
} \
type foo_##type##shuffle_2param_const_random_range (type a, type b) \
{ \
  return __builtin_shuffle (a, b, \
			    (itype) PERM_CONST_RANDOM_RANGE (size)); \
} \
type foo_##type##shuffle_1param_const_random (type a) \
{ \
  return __builtin_shuffle (a, \
			    (itype) PERM_CONST_RANDOM (size)); \
} \
type foo_##type##shuffle_1param_const_random_range (type a) \
{ \
  return __builtin_shuffle (a, \
			    (itype) PERM_CONST_RANDOM_RANGE (size)); \
}

#define SHUFFLE_VEC_INDEX(type, itype) \
type foo##type##itype##shuffle_2param_vec (type a, type b, itype c) \
{ \
  return __builtin_shuffle (a, b, c); \
} \
type foo##type##itype##shuffle_1param_vec (type a, itype c) \
{ \
  return __builtin_shuffle (a, c); \
}

SHUFFLE_CONST_RANDOM (v32hf, v32hi, 32)
SHUFFLE_CONST_RANDOM (v16hf, v16hi, 16)
SHUFFLE_CONST_RANDOM (v8hf, v8hi, 8)

SHUFFLE_VEC_INDEX (v32hf, v32hi)
SHUFFLE_VEC_INDEX (v16hf, v16hi)
SHUFFLE_VEC_INDEX (v8hf, v8hi)
