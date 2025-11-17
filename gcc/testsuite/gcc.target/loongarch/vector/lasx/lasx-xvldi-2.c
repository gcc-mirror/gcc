/* { dg-do compile } */
/* { dg-options "-O2 -mlasx" } */

#include <lasxintrin.h>

#define TEST_V16I16(imm)		  \
void					  \
test_v16i16_##imm (v16i16 *vec)		  \
  {					  \
    v16i16 temp = {imm, imm, imm, imm,	  \
	imm, imm, imm, imm,		  \
	imm, imm, imm, imm,		  \
	imm, imm, imm, imm};		  \
    *vec = temp;			  \
  }

#define TEST_V8I32(imm)			  \
void					  \
test_v8i32_##imm (v8i32 *vec)		  \
  {					  \
    v8i32 temp = {imm, imm, imm, imm,	  \
		  imm, imm, imm, imm};	  \
    *vec = temp;			  \
  }

#define TEST_V8I32_2(imm1, imm2)	  \
void					  \
test_v8i32_2_##imm1 (v8i32 *vec)	  \
  {					  \
    v8i32 temp = {imm1, imm2, imm1, imm2, \
		  imm1, imm2, imm1, imm2};\
    *vec = temp;			  \
  }

#define TEST_V4I64(imm)			  \
void					  \
test_V4I64_##imm (v4i64 *vec)		  \
  {					  \
    v4i64 temp = {imm, imm, imm, imm};	  \
    *vec = temp;			  \
  }

/* 4'b0001:data={2{16'b0,x[7:0],8'b0}}.  */
TEST_V8I32 (0x3a00)
/* { dg-final { scan-assembler-times "test_v8i32_0x3a00:.*\txvldi\t\\\$xr\[0-9\]+,-3782.*-test_v8i32_0x3a00" 1 } } */

/* 4'b0010:data={2{8'b0,x[7:0],16'b0}}.  */
TEST_V8I32 (0x3a0000)
/* { dg-final { scan-assembler-times "test_v8i32_0x3a0000:.*\txvldi\t\\\$xr\[0-9\]+,-3526.*-test_v8i32_0x3a0000" 1 } } */

/* 4'b0011:data={2{x[7:0],24'b0}}.  */
TEST_V8I32 (0x3a000000)
/* { dg-final { scan-assembler-times "test_v8i32_0x3a000000:.*\txvldi\t\\\$xr\[0-9\]+,-3270.*-test_v8i32_0x3a000000" 1 } } */

/* 4'b0101:data={4{x[7:0],8'b0}}.  */
TEST_V16I16 (0x3a00)
/* { dg-final { scan-assembler-times "test_v16i16_0x3a00:.*\txvldi\t\\\$xr\[0-9\]+,-2758.*-test_v16i16_0x3a00" 1 } } */

/* 4'b0110:data={2{16'b0,x[7:0],8'hFF}}.  */
TEST_V8I32 (0x3aff)
/* { dg-final { scan-assembler-times "test_v8i32_0x3aff:.*\txvldi\t\\\$xr\[0-9\]+,-2502.*-test_v8i32_0x3aff" 1 } } */

/* 4'b0111:data={2{8'b0,x[7:0],16'hFFFF}}. */
TEST_V8I32 (0x3affff)
/* { dg-final { scan-assembler-times "test_v8i32_0x3affff:.*\txvldi\t\\\$xr\[0-9\]+,-2246.*-test_v8i32_0x3affff" 1 } } */

/* 4'b1001:data={{8{x[7]}},{8{x[6]}},{8{x[5]}},{8{x[4]}},{8{x[3]}},{8{x[2]}},
 * {8{x[1]}},{8{x[0]}}}.  */
TEST_V4I64 (0xffffff0000ffff00)
/* { dg-final { scan-assembler-times "test_V4I64_0xffffff0000ffff00:.*\txvldi\t\\\$xr\[0-9\]+,-1562.*-test_V4I64_0xffffff0000ffff00" 1 } } */

TEST_V4I64 (0xffff0000ff)
/* { dg-final { scan-assembler-times "test_V4I64_0xffff0000ff:.*\txvldi\t\\\$xr\[0-9\]+,-1767.*-test_V4I64_0xffff0000ff" 1 } } */

TEST_V8I32_2 (0xffffff00, 0);
/* { dg-final { scan-assembler-times "test_v8i32_2_0xffffff00:.*\txvldi\t\\\$xr\[0-9\]+,-1778.*-test_v8i32_2_0xffffff00" 1 } } */

/* 4'b1010:data={2{x[7],~x[6],{5{x[6]}},x[5:0],19'b0}}. */
TEST_V8I32 (0xbf180000)
/* { dg-final { scan-assembler-times "test_v8i32_0xbf180000:.*\txvldi\t\\\$xr\[0-9\]+,-1309.*-test_v8i32_0xbf180000" 1 } } */

TEST_V8I32 (0x41e00000)
/* { dg-final { scan-assembler-times "test_v8i32_0x41e00000:.*\txvldi\t\\\$xr\[0-9\]+,-1476.*-test_v8i32_0x41e00000" 1 } } */

/* 4'b1011:data={32'b0,x[7],~x[6],{5{x[6]}},x[5:0],19'b0}.  */
TEST_V4I64 (0xbe180000)
/* { dg-final { scan-assembler-times "test_V4I64_0xbe180000:.*\txvldi\t\\\$xr\[0-9\]+,-1085.*-test_V4I64_0xbe180000" 1 } } */

TEST_V4I64 (0x41e00000)
/* { dg-final { scan-assembler-times "test_V4I64_0x41e00000:.*\txvldi\t\\\$xr\[0-9\]+,-1220.*-test_V4I64_0x41e00000" 1 } } */

/* 4'b1100:data={x[7],~x[6],{8{x[6]}},x[5:0],48'b0}.  */
TEST_V4I64 (0xbfd5000000000000)
/* { dg-final { scan-assembler-times "test_V4I64_0xbfd5000000000000:.*\txvldi\t\\\$xr\[0-9\]+,-811.*-test_V4I64_0xbfd5000000000000" 1 } } */

TEST_V4I64 (0x4026000000000000)
/* { dg-final { scan-assembler-times "test_V4I64_0x4026000000000000:.*\txvldi\t\\\$xr\[0-9\]+,-986.*-test_V4I64_0x4026000000000000" 1 } } */
