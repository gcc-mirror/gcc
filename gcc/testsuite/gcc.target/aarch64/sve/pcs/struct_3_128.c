/* { dg-options "-O -msve-vector-bits=128" } */
/* { dg-require-effective-target aarch64_little_endian }
/* { dg-final { check-function-bodies "**" "" } } */

#include "struct.h"

#define CONSUME(VAR)				\
  {						\
    register void *ptr_ asm ("x7") = &(VAR);	\
    asm volatile ("" :: "r" (ptr_) : "memory");	\
  }

#define SEL2(TAG, TYPE)				\
  TAG TYPE					\
  sel2_##TYPE (TAG TYPE x, TAG TYPE y)		\
  {						\
    return y;					\
  }

#define WRAP(TYPE)				\
  struct wrap_##TYPE				\
  {						\
    TYPE data;					\
  };						\
  SEL2 (struct, wrap_##TYPE)

/*
** sel2_wrap_fixed_int8_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_int8_t\n} } } */
WRAP (fixed_int8_t);

/*
** sel2_wrap_fixed_int16_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_int16_t\n} } } */
WRAP (fixed_int16_t);

/*
** sel2_wrap_fixed_int32_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_int32_t\n} } } */
WRAP (fixed_int32_t);

/*
** sel2_wrap_fixed_int64_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_int64_t\n} } } */
WRAP (fixed_int64_t);

/*
** sel2_wrap_fixed_uint8_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_uint8_t\n} } } */
WRAP (fixed_uint8_t);

/*
** sel2_wrap_fixed_uint16_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_uint16_t\n} } } */
WRAP (fixed_uint16_t);

/*
** sel2_wrap_fixed_uint32_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_uint32_t\n} } } */
WRAP (fixed_uint32_t);

/*
** sel2_wrap_fixed_uint64_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_uint64_t\n} } } */
WRAP (fixed_uint64_t);

/*
** sel2_wrap_fixed_bfloat16_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_bfloat16_t\n} } } */
WRAP (fixed_bfloat16_t);

/*
** sel2_wrap_fixed_float16_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_float16_t\n} } } */
WRAP (fixed_float16_t);

/*
** sel2_wrap_fixed_float32_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_float32_t\n} } } */
WRAP (fixed_float32_t);

/*
** sel2_wrap_fixed_float64_t:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_float64_t\n} } } */
WRAP (fixed_float64_t);

/*
** sel2_wrap_fixed_bool_t:
**	mov	p0\.b, p1\.b
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_wrap_fixed_bool_t\n} } } */
WRAP (fixed_bool_t);

struct pst_arr1
{
  fixed_uint8_t u8[1];
};

/*
** sel2_pst_arr1:
**	mov	z0\.d, z1\.d
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_arr1\n} } } */
SEL2 (struct, pst_arr1)

/*
** test_pst_arr1:
**	eor	z0\.b, z0\.b, #(?:0x)?1
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr1\n} } } */
svuint8_t
test_pst_arr1 (struct pst_arr1 x)
{
  return sveor_x (svptrue_b8 (), x.u8[0], 1);
}

struct pst_arr2
{
  fixed_uint8_t u8[2];
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_arr2\n} } } */
SEL2 (struct, pst_arr2)

/*
** test_pst_arr2:
**	sub	z0\.b, z0\.b, z1\.b
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr2\n} } } */
svuint8_t
test_pst_arr2 (struct pst_arr2 x)
{
  return svsub_x (svptrue_b8 (), x.u8[0], x.u8[1]);
}

struct pst_arr3
{
  fixed_uint16_t u16[3];
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_arr3\n} } } */
SEL2 (struct, pst_arr3)

/*
** test_pst_arr3:
**	sub	z0\.h, z0\.h, z2\.h
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr3\n} } } */
svuint16_t
test_pst_arr3 (struct pst_arr3 x)
{
  return svsub_x (svptrue_b8 (), x.u16[0], x.u16[2]);
}

struct pst_arr4
{
  fixed_uint32_t u32[4];
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_arr4\n} } } */
SEL2 (struct, pst_arr4)

/*
** test_pst_arr4:
**	sub	z0\.s, z0\.s, z3\.s
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr4\n} } } */
svuint32_t
test_pst_arr4 (struct pst_arr4 x)
{
  return svsub_x (svptrue_b8 (), x.u32[0], x.u32[3]);
}

struct pst_arr5
{
  fixed_uint64_t u64[2][2][2];
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_arr5\n} } } */
SEL2 (struct, pst_arr5)

/*
** test_pst_arr5:
**	sub	sp, sp, #128
** (
**	str	z0, \[sp\]
**	str	z1, \[sp, #1, mul vl\]
**	str	z2, \[sp, #2, mul vl\]
**	str	z3, \[sp, #3, mul vl\]
**	str	z4, \[sp, #4, mul vl\]
**	str	z5, \[sp, #5, mul vl\]
**	str	z6, \[sp, #6, mul vl\]
**	str	z7, \[sp, #7, mul vl\]
** |
**	stp	q0, q1, \[sp\]
**	stp	q2, q3, \[sp, 32\]
**	stp	q4, q5, \[sp, 64\]
**	stp	q6, q7, \[sp, 96\]
** )
**	mov	(x7, sp|w7, wsp)
**	add	sp, sp, #?128
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr5\n} } } */
void
test_pst_arr5 (struct pst_arr5 x)
{
  CONSUME (x);
}

/*
** test_pst_arr5_x0:
** (
**	mov	z0\.d, z7\.d
**	mov	(x7, x0|w7, w0)
** |
**	mov	(x7, x0|w7, w0)
**	mov	z0\.d, z7\.d
** )
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr5_x0\n} } } */
svint32_t
test_pst_arr5_x0 (svint32_t z0, struct pst_arr5 x,
		  svint32_t z1, svint32_t z2, svint32_t z3, svint32_t z4,
		  svint32_t z5, svint32_t z6, svint32_t z7)
{
  CONSUME (x);
  return z7;
}

/*
** test_pst_arr5_x7:
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr5_x7\n} } } */
svint32_t
test_pst_arr5_x7 (svint32_t z0, int x0, int x1, int x2, int x3, int x4,
		  int x5, int x6, struct pst_arr5 x)
{
  CONSUME (x);
  return z0;
}

/*
** test_pst_arr5_sp: { target lp64 }
**	ldr	x7, \[sp\]
**	ret
*/
/*
** test_pst_arr5_sp: { target ilp32 }
**	ldr	w7, \[sp\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr5_sp\n} } } */
svint32_t
test_pst_arr5_sp (svint32_t z0, int x0, int x1, int x2, int x3, int x4,
		  int x5, int x6, int x7, struct pst_arr5 x)
{
  CONSUME (x);
  return z0;
}

struct pst_arr6
{
  fixed_bool_t b[2][2];
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_arr6\n} } } */
SEL2 (struct, pst_arr6)

/*
** test_pst_arr6:
**	...
**	brkpa	p0\.b, p0/z, p2\.b, p3\.b
**	...
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr6\n} } } */
fixed_bool_t
test_pst_arr6 (struct pst_arr6 x)
{
  return svbrkpa_z (x.b[0][0], x.b[1][0], x.b[1][1]);
}

/*
** test_pst_arr6_x0:
** (
**	mov	p0\.b, p3\.b
**	mov	(x7, x0|w7, w0)
** |
**	mov	(x7, x0|w7, w0)
**	mov	p0\.b, p3\.b
** )
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr6_x0\n} } } */
fixed_bool_t
test_pst_arr6_x0 (svbool_t p0, struct pst_arr6 x, svbool_t p1, svbool_t p2,
		  svbool_t p3)
{
  CONSUME (x);
  return p3;
}

/*
** test_pst_arr6_x7:
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr6_x7\n} } } */
fixed_bool_t
test_pst_arr6_x7 (svbool_t p0, int x0, int x1, int x2, int x3, int x4,
		  int x5, int x6, struct pst_arr6 x)
{
  CONSUME (x);
  return p0;
}

/*
** test_pst_arr6_sp: { target lp64 }
**	ldr	x7, \[sp\]
**	ret
*/
/*
** test_pst_arr6_sp: { target ilp32 }
**	ldr	w7, \[sp\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_arr6_sp\n} } } */
fixed_bool_t
test_pst_arr6_sp (svbool_t p0, int x0, int x1, int x2, int x3, int x4,
		  int x5, int x6, int x7, struct pst_arr6 x)
{
  CONSUME (x);
  return p0;
}

struct pst_uniform1
{
  fixed_int8_t a, b;
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_uniform1\n} } } */
SEL2 (struct, pst_uniform1)

/*
** test_pst_uniform1:
**	sub	sp, sp, #32
** (
**	str	z0, \[sp\]
**	str	z1, \[sp, #1, mul vl\]
** |
**	stp	q0, q1, \[sp\]
** )
**	mov	(x7, sp|w7, wsp)
**	add	sp, sp, #?32
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_uniform1\n} } } */
void
test_pst_uniform1 (struct pst_uniform1 x)
{
  CONSUME (x);
}

struct pst_uniform2
{
  fixed_int16_t a;
  fixed_int16_t b[2];
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_uniform2\n} } } */
SEL2 (struct, pst_uniform2)

/*
** test_pst_uniform2:
**	sub	sp, sp, #48
** (
**	str	z0, \[sp\]
**	str	z1, \[sp, #1, mul vl\]
** |
**	stp	q0, q1, \[sp\]
** )
**	str	z2, \[sp, #2, mul vl\]
**	mov	(x7, sp|w7, wsp)
**	add	sp, sp, #?48
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_uniform2\n} } } */
void
test_pst_uniform2 (struct pst_uniform2 x)
{
  CONSUME (x);
}

struct pst_uniform3
{
  fixed_int32_t a;
  fixed_int32_t b[2];
  fixed_int32_t c;
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_uniform3\n} } } */
SEL2 (struct, pst_uniform3)

/*
** test_pst_uniform3:
**	sub	sp, sp, #64
** (
**	str	z0, \[sp\]
**	str	z1, \[sp, #1, mul vl\]
**	str	z2, \[sp, #2, mul vl\]
**	str	z3, \[sp, #3, mul vl\]
** |
**	stp	q0, q1, \[sp\]
**	stp	q2, q3, \[sp, 32\]
** )
**	mov	(x7, sp|w7, wsp)
**	add	sp, sp, #?64
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_uniform3\n} } } */
void
test_pst_uniform3 (struct pst_uniform3 x)
{
  CONSUME (x);
}

struct pst_uniform4
{
  fixed_int32_t a __attribute__((aligned(SVE_BYTES * 2)));
  fixed_int32_t b[3] __attribute__((aligned(SVE_BYTES * 2)));
  fixed_int32_t c __attribute__((aligned(SVE_BYTES * 2)));
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_uniform4\n} } } */
SEL2 (struct, pst_uniform4)

/*
** test_pst_uniform4:
**	sub	sp, sp, #144
**	add	(x[0-9]+), sp, #?31
**	and	x7, \1, #?(?:-32|4294967264)
**	mov	(x[0-9]+), x7
**	str	q0, \[\2\], 32
** (
**	str	z1, \[\2\]
**	str	z2, \[\2, #1, mul vl\]
** |
**	stp	q1, q2, \[\2\]
** )
**	str	z3, \[\2, #2, mul vl\]
**	str	q4, \[x7, 96\]
**	add	sp, sp, #?144
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_uniform4\n} } } */
void
test_pst_uniform4 (struct pst_uniform4 x)
{
  CONSUME (x);
}

struct pst_mixed1
{
  fixed_bool_t p0;
  fixed_bfloat16_t z0;
  fixed_float16_t z1;
  fixed_float32_t z2;
  fixed_float64_t z3;
  fixed_bool_t p1;
  fixed_bool_t p2;
  fixed_int8_t z4;
  fixed_int16_t z5;
  fixed_int32_t z6;
  fixed_int64_t z7;
  fixed_bool_t p3;
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_mixed1\n} } } */
SEL2 (struct, pst_mixed1)

/*
** test_pst_mixed1:
**	sub	sp, sp, #176
**	str	p0, \[sp\]
**	stp	q0, q1, \[sp, 16\]
**	stp	q2, q3, \[sp, 48\]
**	str	p1, \[sp, #40, mul vl\]
**	str	p2, \[sp, #41, mul vl\]
**	stp	q4, q5, \[sp, 96\]
**	stp	q6, q7, \[sp, 128\]
**	str	p3, \[sp, #80, mul vl\]
**	mov	(x7, sp|w7, wsp)
**	add	sp, sp, #?176
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_mixed1\n} } } */
void
test_pst_mixed1 (struct pst_mixed1 x)
{
  CONSUME (x);
}

struct pst_mixed2
{
  struct __attribute__ ((packed)) {
    fixed_bool_t p;
    fixed_int8_t z;
  } a[3];
  fixed_int16_t b[1][1][1][4];
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_mixed2\n} } } */
SEL2 (struct, pst_mixed2)

/*
** test_pst_mixed2:
**	sub	sp, sp, #128
**	str	p0, \[sp\]
**	str	q0, \[sp, 2\]
**	str	p1, \[sp, #9, mul vl\]
**	str	q1, \[sp, 20\]
**	str	p2, \[sp, #18, mul vl\]
**	str	q2, \[sp, 38\]
** (
**	str	z3, \[sp, #4, mul vl\]
**	str	z4, \[sp, #5, mul vl\]
**	str	z5, \[sp, #6, mul vl\]
**	str	z6, \[sp, #7, mul vl\]
** |
**	stp	q3, q4, \[sp, 64\]
**	stp	q5, q6, \[sp, 96\]
** )
**	mov	(x7, sp|w7, wsp)
**	add	sp, sp, #?128
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_mixed2\n} } } */
void
test_pst_mixed2 (struct pst_mixed2 x)
{
  CONSUME (x);
}

struct pst_big1
{
  fixed_int8_t a[9];
};
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tsel2_pst_big1\n} } } */
SEL2 (struct, pst_big1)

/*
** test_pst_big1_a: { target lp64 }
**	ldr	q0, \[x0\]
**	ret
*/
/*
** test_pst_big1_a: { target ilp32 }
**	uxtw	x0, w0
**	ptrue	(p[0-7])\.b, vl16
**	ld1b	z0\.b, \1/z, \[x0\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_big1_a\n} } } */
svint8_t
test_pst_big1_a (struct pst_big1 x)
{
  return x.a[0];
}

/*
** test_pst_big1_b: { target lp64 }
**	add	x7, x0, #?128
**	ret
*/
/*
** test_pst_big1_b: { target ilp32 }
**	add	w7, w0, #?128
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_big1_b\n} } } */
svint8_t
test_pst_big1_b (struct pst_big1 x)
{
  CONSUME (x.a[8]);
}

struct pst_big2
{
  fixed_bool_t a[5];
};
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tsel2_pst_big2\n} } } */
SEL2 (struct, pst_big2)

/*
** test_pst_big2_a: { target lp64 }
**	ldr	p0, \[x0\]
**	ret
*/
/*
** test_pst_big2_a: { target ilp32 }
**	uxtw	x0, w0
**	ldr	p0, \[x0\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_big2_a\n} } } */
svbool_t
test_pst_big2_a (struct pst_big2 x)
{
  return x.a[0];
}

/*
** test_pst_big2_b: { target lp64 }
**	ldr	p0, \[x0, #4, mul vl\]
**	ret
*/
/*
** test_pst_big2_b: { target ilp32 }
**	uxtw	x0, w0
**	ldr	p0, \[x0, #4, mul vl\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_big2_b\n} } } */
svbool_t
test_pst_big2_b (struct pst_big2 x)
{
  return x.a[4];
}

struct pst_big3
{
  fixed_bool_t p0;
  fixed_int8_t a[2];
  fixed_bool_t p1;
  fixed_bool_t p2;
  fixed_bool_t p3;
  fixed_int8_t b[6];
  fixed_bool_t p4;
};
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tsel2_pst_big3\n} } } */
SEL2 (struct, pst_big3)

/*
** test_pst_big3_a: { target lp64 }
**	ldr	p0, \[x0\]
**	ret
*/
/*
** test_pst_big3_a: { target ilp32 }
**	uxtw	x0, w0
**	ldr	p0, \[x0\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_big3_a\n} } } */
svbool_t
test_pst_big3_a (struct pst_big3 x)
{
  return x.p0;
}

/*
** test_pst_big3_b: { target lp64 }
**	ldr	p0, \[x0, #24, mul vl\]
**	ret
*/
/*
** test_pst_big3_b: { target ilp32 }
**	uxtw	x0, w0
**	ldr	p0, \[x0, #24, mul vl\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_big3_b\n} } } */
svbool_t
test_pst_big3_b (struct pst_big3 x)
{
  return x.p1;
}

/*
** test_pst_big3_c: { target lp64 }
**	ldr	p0, \[x0, #25, mul vl\]
**	ret
*/
/*
** test_pst_big3_c: { target ilp32 }
**	uxtw	x0, w0
**	ldr	p0, \[x0, #25, mul vl\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_big3_c\n} } } */
svbool_t
test_pst_big3_c (struct pst_big3 x)
{
  return x.p2;
}

/*
** test_pst_big3_d: { target lp64 }
**	ldr	p0, \[x0, #80, mul vl\]
**	ret
*/
/*
** test_pst_big3_d: { target ilp32 }
**	uxtw	x0, w0
**	ldr	p0, \[x0, #80, mul vl\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_big3_d\n} } } */
svbool_t
test_pst_big3_d (struct pst_big3 x)
{
  return x.p4;
}

/*
** test_pst_big3_e: { target lp64 }
**	ldr	q0, \[x0, 16\]
**	ret
*/
/*
** test_pst_big3_e: { target ilp32 }
**	uxtw	x0, w0
**	ptrue	(p[0-7])\.b, vl16
**	ld1b	z0\.b, \1/z, \[x0, #1, mul vl\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_big3_e\n} } } */
svint8_t
test_pst_big3_e (struct pst_big3 x)
{
  return x.a[0];
}

/*
** test_pst_big3_f: { target lp64 }
**	ldr	q0, \[x0, 80\]
**	ret
*/
/*
** test_pst_big3_f: { target ilp32 }
**	uxtw	x0, w0
**	ptrue	(p[0-7])\.b, vl16
**	ld1b	z0\.b, \1/z, \[x0, #5, mul vl\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_big3_f\n} } } */
svint8_t
test_pst_big3_f (struct pst_big3 x)
{
  return x.b[1];
}

struct pst_zero1
{
  fixed_bool_t a[0];
  fixed_int32_t b;
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_zero1\n} } } */
SEL2 (struct, pst_zero1)

/*
** test_pst_zero1:
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_zero1\n} } } */
svint32_t
test_pst_zero1 (struct pst_zero1 x)
{
  return x.b;
}

struct pst_zero2
{
  unsigned int : 0;
  fixed_bool_t b;
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_zero2\n} } } */
SEL2 (struct, pst_zero2)

/*
** test_pst_zero2:
** (
**	sub	sp, sp, #16
**	add	sp, sp, #?16
** |
** )
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_zero2\n} } } */
svbool_t
test_pst_zero2 (struct pst_zero2 x)
{
  return x.b;
}

struct pst_zero3
{
  struct {} empty;
  fixed_uint64_t b;
};
/* { dg-final { scan-assembler {\t\.variant_pcs\tsel2_pst_zero3\n} } } */
SEL2 (struct, pst_zero3)

/*
** test_pst_zero3:
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_pst_zero3\n} } } */
svuint64_t
test_pst_zero3 (struct pst_zero3 x)
{
  return x.b;
}

typedef unsigned char small_vec __attribute__((vector_size(SVE_BYTES / 4)));

struct nonpst1
{
  small_vec a[4];
};
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tsel2_pst_nonpst1\n} } } */
SEL2 (struct, nonpst1)

/*
** test_nonpst1:
**	...
**	lsr	x0, x1, #?32
**	...
**	ret
*/
/* { dg-final { scan-assembler-not {\t\.variant_pcs\ttest_nonpst1\n} } } */
small_vec
test_nonpst1 (struct nonpst1 x)
{
  return x.a[3];
}

union nonpst2
{
  struct {
    fixed_bool_t a[0];
    fixed_int32_t b;
  };
};
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tsel2_pst_nonpst2\n} } } */
SEL2 (union, nonpst2)

/*
** test_nonpst2:
**	sub	sp, sp, #16
**	stp	x0, x1, \[sp\]
**	...
**	ldr	z0, \[sp\]
**	add	sp, sp, #?16
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_nonpst2\n} } } */
svint32_t
test_nonpst2 (union nonpst2 x)
{
  return x.b;
}

/*
** ret_nonpst2:
**	mov	x0, #?1
**	movk	x0, #?0x3, lsl #?32
**	mov	x1, #?5
**	movk	x1, #?0x7, lsl #?32
**	ret
*/
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tret_nonpst2\n} } } */
union nonpst2
ret_nonpst2 (void)
{
  return (union nonpst2) { { {}, 1, 3, 5, 7 } };
}

union nonpst3
{
  struct {
    unsigned int : 0;
    fixed_bool_t b;
  };
};
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tsel2_pst_nonpst3\n} } } */
SEL2 (union, nonpst3)

/*
** test_nonpst3:
**	sub	sp, sp, #16
**	str	w0, \[sp, #?12\]
**	ldr	p0, \[sp, #6, mul vl\]
**	add	sp, sp, #?16
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_nonpst3\n} } } */
svbool_t
test_nonpst3 (union nonpst3 x)
{
  return x.b;
}

/*
** ret_nonpst3:
**	mov	w0, #?(?:0xffff|65535)
**	ret
*/
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tret_nonpst3\n} } } */
union nonpst3
ret_nonpst3 (void)
{
  return (union nonpst3) { { svptrue_b8 () } };
}

union nonpst4
{
  struct {
    struct {} empty;
    fixed_uint64_t b;
  };
};
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tsel2_pst_nonpst4\n} } } */
SEL2 (union, nonpst4)

/*
** test_nonpst4:
**	sub	sp, sp, #16
**	stp	x0, x1, \[sp\]
**	...
**	ldr	z0, \[sp\]
**	add	sp, sp, #?16
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_nonpst4\n} } } */
svuint64_t
test_nonpst4 (union nonpst4 x)
{
  return x.b;
}

/*
** ret_nonpst4:
**	mov	x0, 1
**	mov	x1, 2
**	ret
*/
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tret_nonpst4\n} } } */
union nonpst4
ret_nonpst4 (void)
{
  return (union nonpst4) { { {}, 1, 2 } };
}

struct nonpst5
{
  union {
    fixed_uint16_t b;
  };
};
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tsel2_pst_nonpst5\n} } } */
SEL2 (struct, nonpst5)

/*
** test_nonpst5:
**	sub	sp, sp, #16
**	stp	x0, x1, \[sp\]
**	...
**	ldr	z0, \[sp\]
**	add	sp, sp, #?16
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_nonpst5\n} } } */
svuint16_t
test_nonpst5 (struct nonpst5 x)
{
  return x.b;
}

struct nonpst6
{
  fixed_uint64_t b;
  fixed_uint64_t *ptr;
};
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tsel2_pst_nonpst6\n} } } */
SEL2 (struct, nonpst6)

/*
** test_nonpst6: { target lp64 }
**	ldr	q0, \[x0\]
**	ret
*/
/*
** test_nonpst6: { target ilp32 }
**	uxtw	x0, w0
**	ptrue	(p[0-3])\.b, vl16
**	ld1d	z0\.d, \1/z, \[x0\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_nonpst6\n} } } */
svuint64_t
test_nonpst6 (struct nonpst6 x)
{
  return x.b;
}

struct nonpst7
{
  fixed_uint64_t b;
  uint32_t foo __attribute__((vector_size(SVE_BYTES)));
};
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tsel2_pst_nonpst7\n} } } */
SEL2 (struct, nonpst7)

/*
** test_nonpst7: { target lp64 }
**	ldr	q0, \[x0\]
**	ret
*/
/*
** test_nonpst7: { target ilp32 }
**	uxtw	x0, w0
**	ptrue	(p[0-3])\.b, vl16
**	ld1d	z0\.d, \1/z, \[x0\]
**	ret
*/
/* { dg-final { scan-assembler {\t\.variant_pcs\ttest_nonpst7\n} } } */
svuint64_t
test_nonpst7 (struct nonpst7 x)
{
  return x.b;
}

typedef unsigned char tiny_vec __attribute__((vector_size(SVE_BYTES / 8)));

struct nonpst8
{
  tiny_vec a;
};
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tsel2_pst_nonpst8\n} } } */
SEL2 (struct, nonpst8)

/*
** test_nonpst8:
**	ubfx	x0, x0, 8, 8
**	ret
*/
/* { dg-final { scan-assembler-not {\t\.variant_pcs\ttest_nonpst8\n} } } */
unsigned int
test_nonpst8 (struct nonpst8 x)
{
  return x.a[1];
}

/*
** ret_nonpst8:
** (
**	sub	sp, sp, #16
**	mov	w0, #?513
**	add	sp, sp, #?16
** |
**	mov	w0, #?513
** )
**	ret
*/
/* { dg-final { scan-assembler-not {\t\.variant_pcs\tret_nonpst8\n} } } */
struct nonpst8
ret_nonpst8 (void)
{
  return (struct nonpst8) { { 1, 2 } };
}
