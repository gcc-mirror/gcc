/* { dg-do run { target { aarch64_sve128_hw } } } */
/* { dg-require-effective-target aarch64_little_endian } */
/* { dg-options "-msve-vector-bits=128" } */

#include "struct.h"

struct pst1
{
  fixed_int8_t v[8];
  fixed_bool_t p[4];
};

ASM_FUNCTION (make_pst1_asm, struct pst1, (),
	      "mov z0.b, #1\n\t"
	      "mov z1.b, #4\n\t"
	      "mov z2.b, #5\n\t"
	      "mov z3.b, #9\n\t"
	      "mov z4.b, #14\n\t"
	      "mov z5.b, #23\n\t"
	      "mov z6.b, #37\n\t"
	      "mov z7.b, #60\n\t"
	      "ptrue p0.b, vl1\n\t"
	      "ptrue p1.b, vl2\n\t"
	      "ptrue p2.b, vl3\n\t"
	      "ptrue p3.b, vl4");

#define LOAD_PST1(PTR) \
  "ld1b z0.b, p0/z, [" PTR ", #0, mul vl]\n\t" \
  "ld1b z1.b, p0/z, [" PTR ", #1, mul vl]\n\t" \
  "ld1b z2.b, p0/z, [" PTR ", #2, mul vl]\n\t" \
  "ld1b z3.b, p0/z, [" PTR ", #3, mul vl]\n\t" \
  "ld1b z4.b, p0/z, [" PTR ", #4, mul vl]\n\t" \
  "ld1b z5.b, p0/z, [" PTR ", #5, mul vl]\n\t" \
  "ld1b z6.b, p0/z, [" PTR ", #6, mul vl]\n\t" \
  "ld1b z7.b, p0/z, [" PTR ", #7, mul vl]\n\t" \
  "incb " PTR ", all, mul #8\n\t" \
  "ldr p0, [" PTR ", #0, mul vl]\n\t" \
  "ldr p1, [" PTR ", #1, mul vl]\n\t" \
  "ldr p2, [" PTR ", #2, mul vl]\n\t" \
  "ldr p3, [" PTR ", #3, mul vl]"

ASM_FUNCTION (passthru_pst1_x0_a,
	      struct pst1, (svbool_t, struct pst1),
	      "incp x0, p0.b\n\t"
	      "sub x0, x0, #11\n\t"
	      "ptrue p0.b\n\t"
	      LOAD_PST1 ("x0"));

ASM_FUNCTION (passthru_pst1_x0_b,
	      struct pst1, (svbool_t, struct pst1, uint64_t),
	      "incp x0, p0.b\n\t"
	      "add x0, x0, x1\n\t"
	      "sub x0, x0, #52\n\t"
	      "ptrue p0.b\n\t"
	      LOAD_PST1 ("x0"));

ASM_FUNCTION (passthru_pst1_x0_c,
	      struct pst1, (svbool_t, struct pst1, svbool_t,
			    svbool_t, svbool_t, svbool_t),
	      "incp x0, p0.b\n\t"
	      "ldr p0, [x1]\n\t"
	      "incp x0, p1.b\n\t"
	      "incp x0, p2.b\n\t"
	      "incp x0, p3.b\n\t"
	      "incp x0, p0.b\n\t"
	      "sub x0, x0, #27\n\t"
	      "ptrue p0.b\n\t"
	      LOAD_PST1 ("x0"));

ASM_FUNCTION (passthru_pst1_x0_d,
	      struct pst1, (svfloat32_t, struct pst1),
	      "ptrue p0.b\n\t"
	      "fmov z1.s, #1.0\n\t"
	      "fcmeq p0.s, p0/z, z0.s, z1.s\n\t"
	      "uzp1 p0.b, p0.b, p0.b\n\t"
	      "uzp1 p0.b, p0.b, p0.b\n\t"
	      LOAD_PST1 ("x0"));

ASM_FUNCTION (passthru_pst1_x0_e,
	      struct pst1, (svfloat32_t, struct pst1, svint32_t,
			    svint32_t, svint32_t, svint32_t,
			    svint32_t, svint32_t, svint32_t),
	      "ptrue p0.b\n\t"
	      "fmov z24.s, #4.0\n\t"
	      "fcmeq p0.s, p0/z, z0.s, z24.s\n\t"
	      "cmpeq p0.s, p0/z, z1.s, #-4\n\t"
	      "cmpeq p0.s, p0/z, z2.s, #-9\n\t"
	      "cmpeq p0.s, p0/z, z3.s, #-14\n\t"
	      "cmpeq p0.s, p0/z, z4.s, #11\n\t"
	      "cmpeq p0.s, p0/z, z5.s, #10\n\t"
	      "cmpeq p0.s, p0/z, z6.s, #8\n\t"
	      "cmpeq p0.s, p0/z, z7.s, #-1\n\t"
	      "uzp1 p0.b, p0.b, p0.b\n\t"
	      "uzp1 p0.b, p0.b, p0.b\n\t"
	      LOAD_PST1 ("x0"));

ASM_FUNCTION (passthru_pst1_x7_a,
	      struct pst1, (svbool_t,
			    uint64_t, uint64_t, uint64_t, uint64_t,
			    uint64_t, uint64_t, uint64_t, struct pst1),
	      "add x0, x0, x1\n\t"
	      "add x2, x2, x3\n\t"
	      "add x4, x4, x5\n\t"
	      "add x0, x0, x2\n\t"
	      "add x4, x4, x6\n\t"
	      "add x0, x0, x4\n\t"
	      "add x7, x7, x0\n\t"
	      "sub x7, x7, #127\n\t"
	      "ptrue p0.b\n\t"
	      LOAD_PST1 ("x7"));

ASM_FUNCTION (passthru_pst1_x7_b,
	      struct pst1, (svbool_t, svbool_t, svbool_t, svbool_t,
			    svbool_t, svbool_t, svbool_t, svbool_t,
			    svbool_t, svbool_t, svbool_t,
			    struct pst1),
	      "and p0.b, p1/z, p0.b, p2.b\n\t"
	      "ldr p2, [x0]\n\t"
	      "and p0.b, p2/z, p0.b, p3.b\n\t"
	      "ldr p2, [x1]\n\t"
	      "ldr p3, [x2]\n\t"
	      "and p0.b, p2/z, p0.b, p3.b\n\t"
	      "ldr p2, [x3]\n\t"
	      "ldr p3, [x4]\n\t"
	      "and p0.b, p2/z, p0.b, p3.b\n\t"
	      "ldr p2, [x5]\n\t"
	      "ldr p3, [x6]\n\t"
	      "and p0.b, p2/z, p0.b, p3.b\n\t"
	      LOAD_PST1 ("x7"));

ASM_FUNCTION (passthru_pst1_sp_a,
	      struct pst1, (svbool_t, svbool_t, svbool_t, svbool_t,
			    svbool_t, svbool_t, svbool_t, svbool_t,
			    svbool_t, svbool_t, svbool_t, svbool_t,
			    struct pst1),
	      "and p0.b, p1/z, p0.b, p2.b\n\t"
	      "ldr p2, [x0]\n\t"
	      "and p0.b, p2/z, p0.b, p3.b\n\t"
	      "ldr p2, [x1]\n\t"
	      "ldr p3, [x2]\n\t"
	      "and p0.b, p2/z, p0.b, p3.b\n\t"
	      "ldr p2, [x3]\n\t"
	      "ldr p3, [x4]\n\t"
	      "and p0.b, p2/z, p0.b, p3.b\n\t"
	      "ldr p2, [x5]\n\t"
	      "ldr p3, [x6]\n\t"
	      "and p0.b, p2/z, p0.b, p3.b\n\t"
	      "ldr p2, [x7]\n\t"
	      "and p0.b, p2/z, p0.b, p0.b\n\t"
	      "ldr x5, [sp]\n\t"
#if __ILP32__
	      "uxtw x5, w5\n\t"
#endif
	      LOAD_PST1 ("x5"));

void
test_vl (svbool_t p0, unsigned int vl)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, sveor_z (pg, p0, svwhilelt_b8 (0U, vl))))
    __builtin_abort ();
}

void
test_pst1 (struct pst1 *x)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, svcmpne (pg, x->v[0], 1))
      || svptest_any (pg, svcmpne (pg, x->v[1], 4))
      || svptest_any (pg, svcmpne (pg, x->v[2], 5))
      || svptest_any (pg, svcmpne (pg, x->v[3], 9))
      || svptest_any (pg, svcmpne (pg, x->v[4], 14))
      || svptest_any (pg, svcmpne (pg, x->v[5], 23))
      || svptest_any (pg, svcmpne (pg, x->v[6], 37))
      || svptest_any (pg, svcmpne (pg, x->v[7], 60))
      || svptest_any (pg, sveor_z (pg, x->p[0], svptrue_pat_b8 (SV_VL1)))
      || svptest_any (pg, sveor_z (pg, x->p[1], svptrue_pat_b8 (SV_VL2)))
      || svptest_any (pg, sveor_z (pg, x->p[2], svptrue_pat_b8 (SV_VL3)))
      || svptest_any (pg, sveor_z (pg, x->p[3], svptrue_pat_b8 (SV_VL4))))
    __builtin_abort ();
}

struct pst1
make_pst1 (void)
{
  struct pst1 res;
  res.v[0] = svdup_s8 (1);
  res.v[1] = svdup_s8 (4);
  res.v[2] = svdup_s8 (5);
  res.v[3] = svdup_s8 (9);
  res.v[4] = svdup_s8 (14);
  res.v[5] = svdup_s8 (23);
  res.v[6] = svdup_s8 (37);
  res.v[7] = svdup_s8 (60);
  res.p[0] = svptrue_pat_b8 (SV_VL1);
  res.p[1] = svptrue_pat_b8 (SV_VL2);
  res.p[2] = svptrue_pat_b8 (SV_VL3);
  res.p[3] = svptrue_pat_b8 (SV_VL4);
  return res;
}

struct pst1
deref_pst1 (struct pst1 *ptr)
{
  return *ptr;
}

void
consume_pst1 (struct pst1 x)
{
  test_pst1 (&x);
}

void
consume_pst1_x0_a (svbool_t p0, struct pst1 x0)
{
  test_vl (p0, 11);
  test_pst1 (&x0);
}

void
consume_pst1_x0_b (svbool_t p0, struct pst1 x0, uint64_t x1)
{
  test_vl (p0, 10);
  test_pst1 (&x0);
  if (x1 != 42)
    __builtin_abort ();
}

void
consume_pst1_x0_c (svbool_t p0, struct pst1 x0, svbool_t p1,
		   svbool_t p2, svbool_t p3, svbool_t x1)
{
  test_vl (p0, 9);
  test_pst1 (&x0);
  test_vl (p1, 7);
  test_vl (p2, 6);
  test_vl (p3, 3);
  test_vl (x1, 2);
}

void
consume_pst1_x0_d (svfloat32_t z0, struct pst1 x0)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, svcmpne (pg, z0, 1.0)))
    __builtin_abort ();
  test_pst1 (&x0);
}

void
consume_pst1_x0_e (svfloat32_t z0, struct pst1 x0,
		   svint32_t z1, svint32_t z2, svint32_t z3, svint32_t z4,
		   svint32_t z5, svint32_t z6, svint32_t z7)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, svcmpne (pg, z0, 4.0))
      || svptest_any (pg, svcmpne (pg, z1, -4))
      || svptest_any (pg, svcmpne (pg, z2, -9))
      || svptest_any (pg, svcmpne (pg, z3, -14))
      || svptest_any (pg, svcmpne (pg, z4, 11))
      || svptest_any (pg, svcmpne (pg, z5, 10))
      || svptest_any (pg, svcmpne (pg, z6, 8))
      || svptest_any (pg, svcmpne (pg, z7, -1)))
    __builtin_abort ();
  test_pst1 (&x0);
}

void
consume_pst1_x7_a (svbool_t p0, uint64_t x0, uint64_t x1, uint64_t x2,
		   uint64_t x3, uint64_t x4, uint64_t x5, uint64_t x6,
		   struct pst1 x7)
{
  test_vl (p0, __ARM_FEATURE_SVE_BITS);
  if (x0 != 1
      || x1 != 2
      || x2 != 4
      || x3 != 8
      || x4 != 16
      || x5 != 32
      || x6 != 64)
    __builtin_abort ();
  test_pst1 (&x7);
}

void
consume_pst1_x7_b (svbool_t p0, svbool_t p1, svbool_t p2, svbool_t p3,
		   svbool_t x0, svbool_t x1, svbool_t x2, svbool_t x3,
		   svbool_t x4, svbool_t x5, svbool_t x6, struct pst1 x7)
{
  test_vl (p0, __ARM_FEATURE_SVE_BITS);
  test_vl (p1, __ARM_FEATURE_SVE_BITS);
  test_vl (p2, __ARM_FEATURE_SVE_BITS);
  test_vl (p3, __ARM_FEATURE_SVE_BITS);
  test_vl (x0, __ARM_FEATURE_SVE_BITS);
  test_vl (x1, __ARM_FEATURE_SVE_BITS);
  test_vl (x2, __ARM_FEATURE_SVE_BITS);
  test_vl (x3, __ARM_FEATURE_SVE_BITS);
  test_vl (x4, __ARM_FEATURE_SVE_BITS);
  test_vl (x5, __ARM_FEATURE_SVE_BITS);
  test_vl (x6, __ARM_FEATURE_SVE_BITS);
  test_pst1 (&x7);
}

void
consume_pst1_sp_a (svbool_t p0, svbool_t p1, svbool_t p2, svbool_t p3,
		   svbool_t x0, svbool_t x1, svbool_t x2, svbool_t x3,
		   svbool_t x4, svbool_t x5, svbool_t x6, svbool_t x7,
		   struct pst1 sp)
{
  test_vl (p0, __ARM_FEATURE_SVE_BITS);
  test_vl (p1, __ARM_FEATURE_SVE_BITS);
  test_vl (p2, __ARM_FEATURE_SVE_BITS);
  test_vl (p3, __ARM_FEATURE_SVE_BITS);
  test_vl (x0, __ARM_FEATURE_SVE_BITS);
  test_vl (x1, __ARM_FEATURE_SVE_BITS);
  test_vl (x2, __ARM_FEATURE_SVE_BITS);
  test_vl (x3, __ARM_FEATURE_SVE_BITS);
  test_vl (x4, __ARM_FEATURE_SVE_BITS);
  test_vl (x5, __ARM_FEATURE_SVE_BITS);
  test_vl (x6, __ARM_FEATURE_SVE_BITS);
  test_vl (x7, __ARM_FEATURE_SVE_BITS);
  test_pst1 (&sp);
}

int
main (void)
{
  svbool_t pg = svptrue_b8 ();
  svbool_t vl2 = svptrue_pat_b8 (SV_VL2);
  svbool_t vl3 = svptrue_pat_b8 (SV_VL3);
  svbool_t vl6 = svptrue_pat_b8 (SV_VL6);
  svbool_t vl7 = svptrue_pat_b8 (SV_VL7);
  svbool_t vl9 = svwhilelt_b8 (0, 9);
  svbool_t vl10 = svwhilelt_b8 (0, 10);
  svbool_t vl11 = svwhilelt_b8 (0, 11);

  CLEANSE; struct pst1 res1 = make_pst1_asm ();
  CLEANSE; test_pst1 (&res1);
  CLEANSE; consume_pst1 (make_pst1 ());

  CLEANSE; struct pst1 res2 = deref_pst1 (&res1);
  CLEANSE; test_pst1 (&res2);
  CLEANSE; consume_pst1 (res2);

  CLEANSE; struct pst1 res3 = passthru_pst1_x0_a (vl11, res1);
  CLEANSE; test_pst1 (&res3);
  CLEANSE; consume_pst1_x0_a (vl11, res3);

  CLEANSE; struct pst1 res4 = passthru_pst1_x0_b (vl10, res1, 42);
  CLEANSE; test_pst1 (&res4);
  CLEANSE; consume_pst1_x0_b (vl10, res4, 42);

  CLEANSE; struct pst1 res5 = passthru_pst1_x0_c (vl9, res1, vl7,
						  vl6, vl3, vl2);
  CLEANSE; test_pst1 (&res5);
  CLEANSE; consume_pst1_x0_c (vl9, res5, vl7,
			      vl6, vl3, vl2);

  CLEANSE; struct pst1 res6 = passthru_pst1_x0_d (svdup_f32 (1.0), res1);
  CLEANSE; test_pst1 (&res6);
  CLEANSE; consume_pst1_x0_d (svdup_f32 (1.0), res6);

  CLEANSE; struct pst1 res7 = passthru_pst1_x0_e (svdup_f32 (4.0), res1,
						  svdup_s32 (-4),
						  svdup_s32 (-9),
						  svdup_s32 (-14),
						  svdup_s32 (11),
						  svdup_s32 (10),
						  svdup_s32 (8),
						  svdup_s32 (-1));
  CLEANSE; test_pst1 (&res7);
  CLEANSE; consume_pst1_x0_e (svdup_f32 (4.0), res1,
			      svdup_s32 (-4),
			      svdup_s32 (-9),
			      svdup_s32 (-14),
			      svdup_s32 (11),
			      svdup_s32 (10),
			      svdup_s32 (8),
			      svdup_s32 (-1));

  CLEANSE; struct pst1 res8 = passthru_pst1_x7_a (pg, 1, 2, 4, 8,
						  16, 32, 64, res1);
  CLEANSE; test_pst1 (&res8);
  CLEANSE; consume_pst1_x7_a (pg, 1, 2, 4, 8,
			      16, 32, 64, res8);

  CLEANSE; struct pst1 res9 = passthru_pst1_x7_b (pg, pg, pg, pg,
						  pg, pg, pg, pg,
						  pg, pg, pg, res1);
  CLEANSE; test_pst1 (&res9);
  CLEANSE; consume_pst1_x7_b (pg, pg, pg, pg,
			      pg, pg, pg, pg,
			      pg, pg, pg, res9);

  CLEANSE; struct pst1 res10 = passthru_pst1_sp_a (pg, pg, pg, pg,
						   pg, pg, pg, pg,
						   pg, pg, pg, pg, res1);
  CLEANSE; test_pst1 (&res10);
  CLEANSE; consume_pst1_sp_a (pg, pg, pg, pg,
			      pg, pg, pg, pg,
			      pg, pg, pg, pg, res10);

  return 0;
}
