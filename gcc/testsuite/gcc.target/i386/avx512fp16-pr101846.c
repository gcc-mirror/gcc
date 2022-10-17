/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-final { scan-assembler-times "vpmovzxwd" "3" } } */
/* { dg-final { scan-assembler-times "vpmovdw" "3" } } */

typedef _Float16 v32hf __attribute__((vector_size (64)));
typedef _Float16 v16hf __attribute__((vector_size (32)));
typedef _Float16 v8hf __attribute__((vector_size (16)));
typedef _Float16 v4hf __attribute__((vector_size (8)));
typedef short v4hi __attribute__((vector_size (8)));
typedef short v8hi __attribute__((vector_size (16)));

#define PERM_CONST_INTERLEAVE_v32hi \
0, 16, 1, 17, 2, 18, 3, 19, \
4, 20, 5, 21, 6, 22, 7, 23, \
8, 24, 9, 25, 10, 26, 11, 27, \
12, 28, 13, 29, 14, 30, 15, 31

#define PERM_CONST_INTERLEAVE_v16hi \
0, 8, 1, 9, 2, 10, 3, 11, \
4, 12, 5, 13, 6, 14, 7, 15

#define PERM_CONST_INTERLEAVE_v8hi \
0, 4, 1, 5, 2, 6, 3, 7

#define PERM_CONST_TRUNCATE_v32hi \
0, 2, 4, 6, 8, 10, 12, 14, \
16, 18, 20, 22, 24, 26, 28, 30

#define PERM_CONST_TRUNCATE_v16hi \
0, 2, 4, 6, 8, 10, 12, 14

#define PERM_CONST_TRUNCATE_v8hi \
0, 2, 4, 6

#define PERM_CONST_INTERLEAVE(size) \
  PERM_CONST_INTERLEAVE_v##size##hi

#define PERM_CONST_TRUNCATE(size) \
  PERM_CONST_TRUNCATE_v##size##hi

#define SHUFFLE_CONST_INTERLEAVE(type, rtype, size) \
rtype foo_##type##shufflevector_const_interleave (type a) \
{ \
  return __builtin_shufflevector (a, (type) {}, \
				  PERM_CONST_INTERLEAVE (size)); \
} \
type foo_##type##shufflevector_const_trunc (rtype a) \
{ \
  return __builtin_shufflevector (a, a, \
				  PERM_CONST_TRUNCATE (size)); \
}

SHUFFLE_CONST_INTERLEAVE (v16hf, v32hf, 32)
SHUFFLE_CONST_INTERLEAVE (v8hf, v16hf, 16)
SHUFFLE_CONST_INTERLEAVE (v4hf, v8hf, 8)
