/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

#define TEST_TBL(name, rettype, tbltype, idxtype, ts) \
  rettype test_ ## name ## _ ## ts (tbltype a, idxtype b) \
	{ \
		return name ## _ ## ts (a, b); \
	}

TEST_TBL (vqtbl2, int8x8_t, int8x16x2_t, uint8x8_t, s8)
TEST_TBL (vqtbl2, uint8x8_t, uint8x16x2_t, uint8x8_t, u8)
TEST_TBL (vqtbl2, poly8x8_t, poly8x16x2_t, uint8x8_t, p8)

TEST_TBL (vqtbl2q, int8x16_t, int8x16x2_t, uint8x16_t, s8)
TEST_TBL (vqtbl2q, uint8x16_t, uint8x16x2_t, uint8x16_t, u8)
TEST_TBL (vqtbl2q, poly8x16_t, poly8x16x2_t, uint8x16_t, p8)

TEST_TBL (vqtbl4, int8x8_t, int8x16x4_t, uint8x8_t, s8)
TEST_TBL (vqtbl4, uint8x8_t, uint8x16x4_t, uint8x8_t, u8)
TEST_TBL (vqtbl4, poly8x8_t, poly8x16x4_t, uint8x8_t, p8)

TEST_TBL (vqtbl4q, int8x16_t, int8x16x4_t, uint8x16_t, s8)
TEST_TBL (vqtbl4q, uint8x16_t, uint8x16x4_t, uint8x16_t, u8)
TEST_TBL (vqtbl4q, poly8x16_t, poly8x16x4_t, uint8x16_t, p8)

#define TEST_TBL3(name, rettype, tbltype, idxtype, ts) \
  rettype test_ ## name ## _ ## ts (idxtype a, tbltype b) \
	{ \
		return name ## _ ## ts (b, a); \
	}

TEST_TBL3 (vqtbl3, int8x8_t, int8x16x3_t, uint8x8_t, s8)
TEST_TBL3 (vqtbl3, uint8x8_t, uint8x16x3_t, uint8x8_t, u8)
TEST_TBL3 (vqtbl3, poly8x8_t, poly8x16x3_t, uint8x8_t, p8)

TEST_TBL3 (vqtbl3q, int8x16_t, int8x16x3_t, uint8x16_t, s8)
TEST_TBL3 (vqtbl3q, uint8x16_t, uint8x16x3_t, uint8x16_t, u8)
TEST_TBL3 (vqtbl3q, poly8x16_t, poly8x16x3_t, uint8x16_t, p8)

/* { dg-final { scan-assembler-not "mov\\t" } } */

/* { dg-final { scan-assembler-times "tbl\\t" 18} }  */
