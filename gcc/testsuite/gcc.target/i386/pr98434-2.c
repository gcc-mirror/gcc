/* { dg-do run } */
/* { dg-options "-O2 -mprefer-vector-width=512 -mavx512vl -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512vl } */

#include "pr98434-1.c"
void test (void);
#define DO_TEST test
#define AVX512VL
#define AVX512BW
#include "avx512-check.h"


typedef char int8;
typedef unsigned char uint8;
typedef short int16;
typedef unsigned short uint16;
typedef long long int64;
typedef unsigned long long uint64;

#define F_EMULATE(TYPE, SIZE, OP, NAME)		\
  __attribute__((noipa, optimize("-fno-tree-vectorize"))) void	\
  emulate_##SIZE##_##TYPE##_##NAME (TYPE *a,	\
				    TYPE *b,	\
				    TYPE *c)	\
  {						\
    int i;					\
    for (i = 0; i < SIZE; i++)			\
      {						\
	a[i] = b[i] OP c[i];			\
      }						\
  }

F_EMULATE (int8,  8, <<, vashl);
F_EMULATE (int8,  8,  >>, vashr);
F_EMULATE (uint8,  8, >>, vlshr);
F_EMULATE (int8,  16, <<, vashl);
F_EMULATE (int8,  16, >>, vashr);
F_EMULATE (uint8,  16, >>, vlshr);
F_EMULATE (int8,  32, <<, vashl);
F_EMULATE (int8,  32, >>, vashr);
F_EMULATE (uint8,  32, >>, vlshr);
F_EMULATE (int16,  8, <<, vashl);
F_EMULATE (int16,  8, >>, vashr);
F_EMULATE (uint16, 8, >>, vlshr);
F_EMULATE (int16,  16, <<, vashl);
F_EMULATE (int16,  16, >>, vashr);
F_EMULATE (uint16, 16, >>, vlshr);
F_EMULATE (int16,  32, <<, vashl);
F_EMULATE (int16,  32, >>, vashr);
F_EMULATE (uint16, 32, >>, vlshr);
F_EMULATE (int64,  2, <<, vashl);
F_EMULATE (int64,  2, >>, vashr);
F_EMULATE (uint64,  2, >>, vlshr);
F_EMULATE (int64,  4, <<, vashl);
F_EMULATE (int64,  4, >>, vashr);
F_EMULATE (uint64,  4, >>, vlshr);
F_EMULATE (int64,  8, <<, vashl);
F_EMULATE (int64,  8, >>, vashr);
F_EMULATE (uint64,  8, >>, vlshr);

#define VSHIFT(VTYPE, NAME, src1, src2)	\
  foo_##VTYPE##_##NAME (src1, src2)

#define EMULATE(SIZE, TYPE, NAME, dst, src1, src2)	\
  emulate_##SIZE##_##TYPE##_##NAME (dst, src1, src2)

#define F_TEST_SHIFT(VTYPE, VTYPEU, TYPE, TYPEU, SIZE)    \
  __attribute__((noipa, optimize("-fno-tree-vectorize"))) void \
  test_##VTYPE ()\
  {\
    TYPE src1[SIZE], src2[SIZE], ref[SIZE];		\
    TYPEU usrc1[SIZE], usrc2[SIZE], uref[SIZE];			\
    VTYPE dst;	     \
    VTYPEU udst;     \
    int i;\
    for (i = 0; i < SIZE; i++)\
    {\
      dst[i] = ref[i] = -i; \
      src1[i] = -(i + SIZE);			\
      src2[i] = i % 8;			\
      udst[i] = uref[i] = i;			\
      usrc1[i] = (i + SIZE);			\
      usrc2[i] = (i % 8);			\
    }\
    EMULATE(SIZE, TYPE, vashl, ref, src1, src2);	\
    dst = VSHIFT(VTYPE, vashl, *((VTYPE* )&src1[0]), *((VTYPE*) &src2[0])); \
    for (i = 0; i < SIZE; i++)\
    {\
      if(dst[i] != ref[i]) __builtin_abort();\
    }\
    EMULATE(SIZE, TYPE, vashr, ref, src1, src2);	\
    dst = VSHIFT(VTYPE, vashr, *((VTYPE* )&src1[0]), *((VTYPE*) &src2[0])); \
    for (i = 0; i < SIZE; i++)\
    {\
      if(dst[i] != ref[i]) __builtin_abort();\
    }\
    EMULATE(SIZE, TYPEU, vlshr, uref, usrc1, usrc2);	\
    udst = VSHIFT(VTYPEU, vlshr, *((VTYPEU* )&usrc1[0]), *((VTYPEU*) &usrc2[0])); \
    for (i = 0; i < SIZE; i++)\
    {\
      if(udst[i] != uref[i]) __builtin_abort();\
    }\
  }

F_TEST_SHIFT (v8qi, v8uqi, int8, uint8, 8);
F_TEST_SHIFT (v16qi, v16uqi, int8, uint8, 16);
F_TEST_SHIFT (v32qi, v32uqi, int8, uint8, 32);
F_TEST_SHIFT (v8hi, v8uhi, int16, uint16, 8);
F_TEST_SHIFT (v16hi, v16uhi, int16, uint16, 16);
F_TEST_SHIFT (v32hi, v32uhi, int16, uint16, 32);
F_TEST_SHIFT (v2di, v2udi, int64, uint64, 2);
F_TEST_SHIFT (v4di, v4udi, int64, uint64, 4);
F_TEST_SHIFT (v8di, v8udi, int64, uint64, 8);


void
test (void)
{
  test_v8qi ();
  test_v16qi ();
  test_v32qi ();
  test_v8hi ();
  test_v16hi ();
  test_v32hi ();
  test_v2di ();
  test_v4di ();
  test_v8di ();
}
