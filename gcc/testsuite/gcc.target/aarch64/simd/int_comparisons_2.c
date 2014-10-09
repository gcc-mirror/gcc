/* { dg-do run } */
/* { dg-options "-O2 -fno-inline" } */
/* Stops the test_xxx methods being inlined into main, thus preventing constant
   propagation.  */

#include "int_comparisons.x"

extern void abort (void);

#define CHECK2(R0, R1) if (res[0] != R0 || res[1] != R1) abort ()

#define TEST2(BASETYPE, SUFFIX, RESTYPE, ST1_SUFFIX) {			\
  BASETYPE##_t _a[2] = {2, 3};						\
  BASETYPE##x2_t a = vld1##SUFFIX (_a);					\
  BASETYPE##_t _b[2] = {1, 3};						\
  BASETYPE##x2_t b = vld1##SUFFIX (_b);					\
  RESTYPE res[2];							\
  vst1##ST1_SUFFIX (res, test_vclt##SUFFIX (a, b)); CHECK2 (0, 0);	\
  vst1##ST1_SUFFIX (res, test_vclt##SUFFIX (b, a)); CHECK2 (-1, 0);	\
  vst1##ST1_SUFFIX (res, test_vcle##SUFFIX (a, b)); CHECK2 (0, -1);	\
  vst1##ST1_SUFFIX (res, test_vcle##SUFFIX (b, a)); CHECK2 (-1, -1);	\
  vst1##ST1_SUFFIX (res, test_vceq##SUFFIX (a, b)); CHECK2 (0, -1);	\
  vst1##ST1_SUFFIX (res, test_vcge##SUFFIX (a, b)); CHECK2 (-1, -1);	\
  vst1##ST1_SUFFIX (res, test_vcge##SUFFIX (b, a)); CHECK2 (0, -1);	\
  vst1##ST1_SUFFIX (res, test_vcgt##SUFFIX (a, b)); CHECK2 (-1, 0);	\
  vst1##ST1_SUFFIX (res, test_vcgt##SUFFIX (b, a)); CHECK2 (0, 0);	\
  vst1##ST1_SUFFIX (res, test_vtst##SUFFIX (a, b)); CHECK2 (0, -1);	\
  vst1##ST1_SUFFIX (res, test_vtst##SUFFIX (a + 1, b)); CHECK2 (-1, 0); \
}

#define CHECK4(T, R0, R1, R2, R3)		\
  if (res[0] != (T)R0 || res[1] != (T)R1	\
      || res[2] != (T)R2 || res[3] != (T)R3) abort ()

#define TEST4(BASETYPE, SUFFIX, RESTYPE, ST1_SUFFIX) {	\
  BASETYPE##_t _a[4] = {1, 2, 3, 4};			\
  BASETYPE##x4_t a = vld1##SUFFIX (_a);			\
  BASETYPE##_t _b[4] = {4, 2, 1, 3};			\
  BASETYPE##x4_t b = vld1##SUFFIX (_b);			\
  RESTYPE res[4];					\
  vst1##ST1_SUFFIX (res, test_vclt##SUFFIX (a, b));	\
  CHECK4 (RESTYPE, -1, 0, 0, 0);			\
  vst1##ST1_SUFFIX (res, test_vcle##SUFFIX (a, b));	\
  CHECK4 (RESTYPE, -1, -1, 0, 0);			\
  vst1##ST1_SUFFIX (res, test_vceq##SUFFIX (a, b));	\
  CHECK4 (RESTYPE, 0, -1, 0, 0);			\
  vst1##ST1_SUFFIX (res, test_vcge##SUFFIX (a, b));	\
  CHECK4 (RESTYPE, 0, -1, -1, -1);			\
  vst1##ST1_SUFFIX (res, test_vcgt##SUFFIX (a, b));	\
  CHECK4 (RESTYPE, 0, 0, -1, -1);			\
  vst1##ST1_SUFFIX (res, test_vtst##SUFFIX (a, b));	\
  CHECK4 (RESTYPE, 0, -1, -1, 0);			\
}

#define CHECK8(T, R0, R1, R2, R3, R4, R5, R6, R7)			       \
  if (res[0] != (T)R0 || res[1] != (T)R1 || res[2] != (T)R2 || res[3] != (T)R3 \
      || res[4] != (T)R4 || res[5] != (T)R5 || res[6] != (T)R6		       \
      || res[7] != (T)R7) abort ()

#define TEST8(BASETYPE, SUFFIX, RESTYPE, ST1_SUFFIX) {	\
  BASETYPE##_t _a[8] = {1, 2, 3, 4, 5, 6, 7, 8};	\
  BASETYPE##x8_t a = vld1##SUFFIX (_a);			\
  BASETYPE##_t _b[8] = {4, 2, 1, 3, 2, 6, 8, 9};	\
  BASETYPE##x8_t b = vld1##SUFFIX (_b);			\
  RESTYPE res[8];					\
  vst1##ST1_SUFFIX (res, test_vclt##SUFFIX (a, b));	\
  CHECK8 (RESTYPE, -1, 0, 0, 0, 0, 0, -1, -1);		\
  vst1##ST1_SUFFIX (res, test_vcle##SUFFIX (a, b));	\
  CHECK8 (RESTYPE, -1, -1, 0, 0, 0, -1, -1, -1);	\
  vst1##ST1_SUFFIX (res, test_vceq##SUFFIX (a, b));	\
  CHECK8 (RESTYPE, 0, -1, 0, 0, 0, -1, 0, 0);		\
  vst1##ST1_SUFFIX (res, test_vcge##SUFFIX (a, b));	\
  CHECK8 (RESTYPE, 0, -1, -1, -1, -1, -1, 0, 0);	\
  vst1##ST1_SUFFIX (res, test_vcgt##SUFFIX (a, b));	\
  CHECK8 (RESTYPE, 0, 0, -1, -1, -1, 0, 0, 0);		\
  vst1##ST1_SUFFIX (res, test_vtst##SUFFIX (a, b));	\
  CHECK8 (RESTYPE, 0, -1, -1, 0, 0, -1, 0, -1);		\
}

/* 16-way tests use same 8 values twice.  */
#define CHECK16(T, R0, R1, R2, R3, R4, R5, R6, R7)			       \
  if (res[0] != (T)R0 || res[1] != (T)R1 || res[2] != (T)R2 || res[3] != (T)R3 \
      || res[4] != (T)R4 || res[5] != (T)R5 || res[6] != (T)R6		       \
      || res[7] != (T)R7 || res[8] != (T)R0 || res[9] != (T)R1		       \
      || res[10] != (T)R2 || res[11] != (T)R3 || res[12] != (T)R4	       \
      || res[13] != (T)R5 || res[14] != (T)R6 || res[15] != (T)R7) abort ()

#define TEST16(BASETYPE, SUFFIX, RESTYPE, ST1_SUFFIX) {			  \
  BASETYPE##_t _a[16] = {1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8}; \
  BASETYPE##x16_t a = vld1##SUFFIX (_a);				  \
  BASETYPE##_t _b[16] = {4, 2, 1, 3, 2, 6, 8, 9, 4, 2, 1, 3, 2, 6, 8, 9}; \
  BASETYPE##x16_t b = vld1##SUFFIX (_b);				  \
  RESTYPE res[16];							  \
  vst1##ST1_SUFFIX (res, test_vclt##SUFFIX (a, b));			  \
  CHECK16 (RESTYPE, -1, 0, 0, 0, 0, 0, -1, -1);				  \
  vst1##ST1_SUFFIX (res, test_vcle##SUFFIX (a, b));			  \
  CHECK16 (RESTYPE, -1, -1, 0, 0, 0, -1, -1, -1);			  \
  vst1##ST1_SUFFIX (res, test_vceq##SUFFIX (a, b));			  \
  CHECK16 (RESTYPE, 0, -1, 0, 0, 0, -1, 0, 0);				  \
  vst1##ST1_SUFFIX (res, test_vcge##SUFFIX (a, b));			  \
  CHECK16 (RESTYPE, 0, -1, -1, -1, -1, -1, 0, 0);			  \
  vst1##ST1_SUFFIX (res, test_vcgt##SUFFIX (a, b));			  \
  CHECK16 (RESTYPE, 0, 0, -1, -1, -1, 0, 0, 0);				  \
  vst1##ST1_SUFFIX (res, test_vtst##SUFFIX (a, b));			  \
  CHECK16 (RESTYPE, 0, -1, -1, 0, 0, -1, 0, -1);			  \
}

int
main (int argc, char **argv)
{
  TEST2 (int32, _s32, uint32_t, _u32);
  TEST2 (uint32, _u32, uint32_t, _u32);
  TEST2 (int64, q_s64, uint64_t, q_u64);
  TEST2 (uint64, q_u64, uint64_t, q_u64);

  TEST4 (int16, _s16, uint16_t, _u16);
  TEST4 (uint16, _u16, uint16_t, _u16);
  TEST4 (int32, q_s32, uint32_t, q_u32);
  TEST4 (uint32, q_u32, uint32_t, q_u32);

  TEST8 (int8, _s8, uint8_t, _u8);
  TEST8 (uint8, _u8, uint8_t, _u8);
  TEST8 (int16, q_s16, uint16_t, q_u16);
  TEST8 (uint16, q_u16, uint16_t, q_u16);

  TEST16 (int8, q_s8, uint8_t, q_u8);
  TEST16 (uint8, q_u8, uint8_t, q_u8);

  return 0;
}

