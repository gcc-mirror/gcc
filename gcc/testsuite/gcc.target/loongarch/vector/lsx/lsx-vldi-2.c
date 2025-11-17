/* { dg-do compile } */
/* { dg-options "-O2 -mlasx" } */

#include <lsxintrin.h>

#define TEST_V8I16(imm)			  \
void					  \
test_v8i16_##imm (v8i16 *vec)		  \
  {					  \
    v8i16 temp = {imm, imm, imm, imm,	  \
	    imm, imm, imm, imm};	  \
    *vec = temp;			  \
  }

#define TEST_V4I32(imm)			  \
void					  \
test_v4i32_##imm (v4i32 *vec)		  \
  {					  \
    v4i32 temp = {imm, imm, imm, imm};	  \
    *vec = temp;			  \
  }

#define TEST_V4I32_2(imm1, imm2)	  \
void					  \
test_v4i32_2_##imm1 (v4i32 *vec)	  \
  {					  \
    v4i32 temp = {imm1, imm2, imm1, imm2};\
    *vec = temp;			  \
  }

#define TEST_V2I64(imm)			  \
void					  \
test_v2i64_##imm (v2i64 *vec)		  \
  {					  \
    v2i64 temp = {imm, imm};		  \
    *vec = temp;			  \
  }

/* 4'b0001:data={2{16'b0,x[7:0],8'b0}}.  */
TEST_V4I32 (0x3a00)
/* { dg-final { scan-assembler-times "test_v4i32_0x3a00:.*\tvldi\t\\\$vr\[0-9\]+,-3782.*-test_v4i32_0x3a00" 1 } } */

/* 4'b0010:data={2{8'b0,x[7:0],16'b0}}.  */
TEST_V4I32 (0x3a0000)
/* { dg-final { scan-assembler-times "test_v4i32_0x3a0000:.*\tvldi\t\\\$vr\[0-9\]+,-3526.*-test_v4i32_0x3a0000" 1 } } */

/* 4'b0011:data={2{x[7:0],24'b0}}.  */
TEST_V4I32 (0x3a000000)
/* { dg-final { scan-assembler-times "test_v4i32_0x3a000000:.*\tvldi\t\\\$vr\[0-9\]+,-3270.*-test_v4i32_0x3a000000" 1 } } */

/* 4'b0101:data={4{x[7:0],8'b0}}.  */
TEST_V8I16 (0x3a00)
/* { dg-final { scan-assembler-times "test_v8i16_0x3a00:.*\tvldi\t\\\$vr\[0-9\]+,-2758.*-test_v8i16_0x3a00" 1 } } */

/* 4'b0110:data={2{16'b0,x[7:0],8'hFF}}.  */
TEST_V4I32 (0x3aff)
/* { dg-final { scan-assembler-times "test_v4i32_0x3aff:.*\tvldi\t\\\$vr\[0-9\]+,-2502.*-test_v4i32_0x3aff" 1 } } */

/* 4'b0111:data={2{8'b0,x[7:0],16'hFFFF}}. */
TEST_V4I32 (0x3affff)
/* { dg-final { scan-assembler-times "test_v4i32_0x3affff:.*\tvldi\t\\\$vr\[0-9\]+,-2246.*-test_v4i32_0x3affff" 1 } } */

/* 4'b1001:data={{8{x[7]}},{8{x[6]}},{8{x[5]}},{8{x[4]}},{8{x[3]}},{8{x[2]}},
 * {8{x[1]}},{8{x[0]}}}.  */
TEST_V2I64 (0xffffff0000ffff00)
/* { dg-final { scan-assembler-times "test_v2i64_0xffffff0000ffff00:.*\tvldi\t\\\$vr\[0-9\]+,-1562.*-test_v2i64_0xffffff0000ffff00" 1 } } */

TEST_V2I64 (0xffff0000ff)
/* { dg-final { scan-assembler-times "test_v2i64_0xffff0000ff:.*\tvldi\t\\\$vr\[0-9\]+,-1767.*-test_v2i64_0xffff0000ff" 1 } } */

TEST_V4I32_2 (0xffffff00, 0);
/* { dg-final { scan-assembler-times "test_v4i32_2_0xffffff00:.*\tvldi\t\\\$vr\[0-9\]+,-1778.*-test_v4i32_2_0xffffff00" 1 } } */

/* 4'b1010:data={2{x[7],~x[6],{5{x[6]}},x[5:0],19'b0}}. */
TEST_V4I32 (0xbf180000)
/* { dg-final { scan-assembler-times "test_v4i32_0xbf180000:.*\tvldi\t\\\$vr\[0-9\]+,-1309.*-test_v4i32_0xbf180000" 1 } } */

TEST_V4I32 (0x41e00000)
/* { dg-final { scan-assembler-times "test_v4i32_0x41e00000:.*\tvldi\t\\\$vr\[0-9\]+,-1476.*-test_v4i32_0x41e00000" 1 } } */

/* 4'b1011:data={32'b0,x[7],~x[6],{5{x[6]}},x[5:0],19'b0}.  */
TEST_V2I64 (0xbe180000)
/* { dg-final { scan-assembler-times "test_v2i64_0xbe180000:.*\tvldi\t\\\$vr\[0-9\]+,-1085.*-test_v2i64_0xbe180000" 1 } } */

TEST_V2I64 (0x41e00000)
/* { dg-final { scan-assembler-times "test_v2i64_0x41e00000:.*\tvldi\t\\\$vr\[0-9\]+,-1220.*-test_v2i64_0x41e00000" 1 } } */

/* 4'b1100:data={x[7],~x[6],{8{x[6]}},x[5:0],48'b0}.  */
TEST_V2I64 (0xbfd5000000000000)
/* { dg-final { scan-assembler-times "test_v2i64_0xbfd5000000000000:.*\tvldi\t\\\$vr\[0-9\]+,-811.*-test_v2i64_0xbfd5000000000000" 1 } } */

TEST_V2I64 (0x4026000000000000)
/* { dg-final { scan-assembler-times "test_v2i64_0x4026000000000000:.*\tvldi\t\\\$vr\[0-9\]+,-986.*-test_v2i64_0x4026000000000000" 1 } } */
