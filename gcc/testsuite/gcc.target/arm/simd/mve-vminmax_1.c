/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_1m_mve_ok }  */
/* { dg-options "-O3" }  */
/* { dg-add-options arm_v8_1m_mve }  */

#include <stdint.h>

#define MAX(a, b) ((a) > (b)) ? (a) : (b)
#define MIN(a, b) ((a) < (b)) ? (a) : (b)


#define TEST_BINOP(OP, TY, N)		\
  TY test_##OP##_##TY (TY * dest, TY * a, TY * b)	\
  {							\
    int i;						\
    for (i=0; i<N; i++)					\
    {							\
      dest[i] = OP (a[i], b[i]);			\
    }							\
  }

/* Test vmax.  */

TEST_BINOP (MAX, int32_t, 4)
/* { dg-final { scan-assembler-times {vmax\.s32\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TEST_BINOP (MAX, uint32_t, 4)
/* { dg-final { scan-assembler-times {vmax\.u32\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TEST_BINOP (MAX, int16_t, 8)
/* { dg-final { scan-assembler-times {vmax\.s16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TEST_BINOP (MAX, uint16_t, 8)
/* { dg-final { scan-assembler-times {vmax\.u16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TEST_BINOP (MAX, int8_t, 16)
/* { dg-final { scan-assembler-times {vmax\.s8\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TEST_BINOP (MAX, uint8_t, 16)
/* { dg-final { scan-assembler-times {vmax\.u8\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

/* Test vmin.  */

TEST_BINOP (MIN, int32_t, 4)
/* { dg-final { scan-assembler-times {vmin\.s32\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TEST_BINOP (MIN, uint32_t, 4)
/* { dg-final { scan-assembler-times {vmin\.u32\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TEST_BINOP (MIN, int16_t, 8)
/* { dg-final { scan-assembler-times {vmin\.s16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TEST_BINOP (MIN, uint16_t, 8)
/* { dg-final { scan-assembler-times {vmin\.u16\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TEST_BINOP (MIN, int8_t, 16)
/* { dg-final { scan-assembler-times {vmin\.s8\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

TEST_BINOP (MIN, uint8_t, 16)
/* { dg-final { scan-assembler-times {vmin\.u8\tq[0-9]+, q[0-9]+, q[0-9]+} 1 } }  */

