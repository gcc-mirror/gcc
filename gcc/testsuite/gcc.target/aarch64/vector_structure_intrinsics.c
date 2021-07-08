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

#define TEST_TBX2(name, rettype, tbltype, idxtype, ts) \
  rettype test_ ## name ## _ ## ts (rettype a, idxtype b, tbltype c) \
	{ \
		return name ## _ ## ts (a, c, b); \
	}

TEST_TBX2 (vqtbx2, int8x8_t, int8x16x2_t, uint8x8_t, s8)
TEST_TBX2 (vqtbx2, uint8x8_t, uint8x16x2_t, uint8x8_t, u8)
TEST_TBX2 (vqtbx2, poly8x8_t, poly8x16x2_t, uint8x8_t, p8)

TEST_TBX2 (vqtbx2q, int8x16_t, int8x16x2_t, uint8x16_t, s8)
TEST_TBX2 (vqtbx2q, uint8x16_t, uint8x16x2_t, uint8x16_t, u8)
TEST_TBX2 (vqtbx2q, poly8x16_t, poly8x16x2_t, uint8x16_t, p8)

#define TEST_TBX3(name, rettype, tbltype, idxtype, ts) \
  rettype test_ ## name ## _ ## ts (rettype a, tbltype b, idxtype c) \
	{ \
		return name ## _ ## ts (a, b, c); \
	}

TEST_TBX3 (vqtbx3, int8x8_t, int8x16x3_t, uint8x8_t, s8)
TEST_TBX3 (vqtbx3, uint8x8_t, uint8x16x3_t, uint8x8_t, u8)
TEST_TBX3 (vqtbx3, poly8x8_t, poly8x16x3_t, uint8x8_t, p8)

TEST_TBX3 (vqtbx3q, int8x16_t, int8x16x3_t, uint8x16_t, s8)
TEST_TBX3 (vqtbx3q, uint8x16_t, uint8x16x3_t, uint8x16_t, u8)
TEST_TBX3 (vqtbx3q, poly8x16_t, poly8x16x3_t, uint8x16_t, p8)

#define TEST_TBX4(name, rettype, tbltype, idxtype, ts) \
  rettype test_ ## name ## _ ## ts (rettype a, idxtype b, idxtype dummy1, \
				    idxtype dummy2, tbltype c) \
	{ \
		return name ## _ ## ts (a, c, b); \
	}

TEST_TBX4 (vqtbx4, int8x8_t, int8x16x4_t, uint8x8_t, s8)
TEST_TBX4 (vqtbx4, uint8x8_t, uint8x16x4_t, uint8x8_t, u8)
TEST_TBX4 (vqtbx4, poly8x8_t, poly8x16x4_t, uint8x8_t, p8)

TEST_TBX4 (vqtbx4q, int8x16_t, int8x16x4_t, uint8x16_t, s8)
TEST_TBX4 (vqtbx4q, uint8x16_t, uint8x16x4_t, uint8x16_t, u8)
TEST_TBX4 (vqtbx4q, poly8x16_t, poly8x16x4_t, uint8x16_t, p8)

/* { dg-final { scan-assembler-not "mov\\t" } } */

/* { dg-final { scan-assembler-times "tbl\\t" 18} }  */
/* { dg-final { scan-assembler-times "tbx\\t" 18} }  */
