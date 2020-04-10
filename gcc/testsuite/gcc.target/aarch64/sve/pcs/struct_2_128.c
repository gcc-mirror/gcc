/* { dg-do run { target { aarch64_sve128_hw } } } */
/* { dg-require-effective-target aarch64_little_endian } */
/* { dg-options "-msve-vector-bits=128" } */

#include "struct.h"

struct pst1
{
  fixed_uint32_t u32;
  fixed_uint64_t u64;
};

ASM_FUNCTION (make_pst1_asm, struct pst1, (),
	      "mov z0.s, #0x1ffffe00\n\t"
	      "mov z1.d, #0x7f80");

ASM_FUNCTION (passthru_pst1_asm, struct pst1, (struct pst1), "");

ASM_FUNCTION (passthru_pst1_z6_asm,
	      struct pst1, (svint32_t, svint32_t, svint32_t, svint32_t,
			    svint32_t, svint32_t, struct pst1),
	      "mov z0.d, z6.d\n\t"
	      "mov z1.d, z7.d");

ASM_FUNCTION (passthru_pst1_x0_asm,
	      struct pst1, (svint32_t, svint32_t, svint32_t, svint32_t,
			    svint32_t, svint32_t, svint32_t, struct pst1),
	      "ptrue p0.b\n\t"
	      "ld1w z0.s, p0/z, [x0]\n\t"
	      "ld1d z1.d, p0/z, [x0, #1, mul vl]");

void
test_pst1 (struct pst1 *x)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, svcmpne (pg, x->u32, 0x1ffffe00))
      || svptest_any (pg, svcmpne (pg, x->u64, 0x7f80)))
    __builtin_abort ();
}

struct pst1 deref_pst1 (struct pst1 *ptr) { return *ptr; }
struct pst1 passthru_pst1 (struct pst1 x) { return x; }

struct pst1
passthru_pst1_z6 (svint32_t z0, svint32_t z1, svint32_t z2, svint32_t z3,
		  svint32_t z4, svint32_t z5, struct pst1 z6)
{
  return z6;
}

struct pst1
passthru_pst1_x0 (svint32_t z0, svint32_t z1, svint32_t z2, svint32_t z3,
		  svint32_t z4, svint32_t z5, svint32_t z6, struct pst1 x0)
{
  return x0;
}

void consume_pst1 (struct pst1 x) { test_pst1 (&x); }

static void
run_pst1_tests (void)
{
  svint32_t s32 = svdup_s32 (0);
  svbool_t pg = svptrue_b8 ();

  CLEANSE; struct pst1 res = make_pst1_asm ();
  CLEANSE; test_pst1 (&res);
  CLEANSE; consume_pst1 (deref_pst1 (&res));
  CLEANSE; consume_pst1 (passthru_pst1_asm (res));
  CLEANSE; consume_pst1 (passthru_pst1 (res));
  CLEANSE; consume_pst1 (passthru_pst1_z6_asm (s32, s32, s32, s32,
					       s32, s32, res));
  CLEANSE; consume_pst1 (passthru_pst1_z6 (s32, s32, s32, s32,
					   s32, s32, res));
  CLEANSE; consume_pst1 (passthru_pst1_x0_asm (s32, s32, s32, s32,
					       s32, s32, s32, res));
  CLEANSE; consume_pst1 (passthru_pst1_x0 (s32, s32, s32, s32,
					   s32, s32, s32, res));
}

//--------------------------------------------------------------------------

struct pst2
{
  fixed_uint8_t u8;
  fixed_uint16_t u16;
  struct {
    fixed_float64_t f64;
    fixed_bool_t pg;
  } a[4];
  struct pst1 sub;
};

ASM_FUNCTION (make_pst2_asm, struct pst2, (),
	      "mov z0.b, #100\n\t"
	      "mov z1.h, #99\n\t"
	      "fmov z2.d, #1.0\n\t"
	      "fmov z3.d, #2.0\n\t"
	      "fmov z4.d, #3.0\n\t"
	      "fmov z5.d, #4.0\n\t"
	      "mov z6.s, #98\n\t"
	      "mov z7.d, #97\n\t"
	      "ptrue p0.b, vl5\n\t"
	      "ptrue p1.b, vl6\n\t"
	      "ptrue p2.b, vl7\n\t"
	      "ptrue p3.b, vl8");

ASM_FUNCTION (passthru_pst2_asm, struct pst2, (struct pst2), "");

ASM_FUNCTION (passthru_pst2_x0_asm, struct pst2, (svbool_t, struct pst2),
	      "cntd x2, all, mul #9\n\t"
	      "add x2, x2, #15\n\t"
	      "and x2, x2, #-16\n\t"
	      "ptrue p0.b\n\t"
	      "ld1b z0.b, p0/z, [x0, #0, mul vl]\n\t"
	      "ld1h z1.h, p0/z, [x0, #1, mul vl]\n\t"
	      "ld1d z2.d, p0/z, [x0, #2, mul vl]\n\t"
	      "add x1, x0, x2\n\t"
	      "ld1d z3.d, p0/z, [x1, #2, mul vl]\n\t"
	      "ldr p1, [x1, #24, mul vl]\n\t"
	      "add x1, x1, x2\n\t"
	      "ld1d z4.d, p0/z, [x1, #2, mul vl]\n\t"
	      "ldr p2, [x1, #24, mul vl]\n\t"
	      "add x1, x1, x2\n\t"
	      "ld1d z5.d, p0/z, [x1, #2, mul vl]\n\t"
	      "ldr p3, [x1, #24, mul vl]\n\t"
	      "add x1, x1, x2\n\t"
	      "ld1w z6.s, p0/z, [x1, #2, mul vl]\n\t"
	      "ld1d z7.d, p0/z, [x1, #3, mul vl]\n\t"
	      "ldr p0, [x0, #24, mul vl]");

void
test_pst2 (struct pst2 *x)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, svcmpne (pg, x->u8, 100))
      || svptest_any (pg, svcmpne (pg, x->u16, 99))
      || svptest_any (pg, svcmpne (pg, x->a[0].f64, 1.0))
      || svptest_any (pg, sveor_z (pg, x->a[0].pg, svptrue_pat_b8 (SV_VL5)))
      || svptest_any (pg, svcmpne (pg, x->a[1].f64, 2.0))
      || svptest_any (pg, sveor_z (pg, x->a[1].pg, svptrue_pat_b8 (SV_VL6)))
      || svptest_any (pg, svcmpne (pg, x->a[2].f64, 3.0))
      || svptest_any (pg, sveor_z (pg, x->a[2].pg, svptrue_pat_b8 (SV_VL7)))
      || svptest_any (pg, svcmpne (pg, x->a[3].f64, 4.0))
      || svptest_any (pg, sveor_z (pg, x->a[3].pg, svptrue_pat_b8 (SV_VL8)))
      || svptest_any (pg, svcmpne (pg, x->sub.u32, 98))
      || svptest_any (pg, svcmpne (pg, x->sub.u64, 97)))
    __builtin_abort ();
}

struct pst2 deref_pst2 (struct pst2 *ptr) { return *ptr; }
struct pst2 passthru_pst2 (struct pst2 x) { return x; }

struct pst2
passthru_pst2_x0 (svbool_t pg, struct pst2 x0)
{
  return x0;
}

void
consume_pst2 (struct pst2 x)
{
  test_pst2 (&x);
}

static void
run_pst2_tests (void)
{
  CLEANSE; struct pst2 res = make_pst2_asm ();
  CLEANSE; test_pst2 (&res);
  CLEANSE; consume_pst2 (deref_pst2 (&res));
  CLEANSE; consume_pst2 (passthru_pst2_asm (res));
  CLEANSE; consume_pst2 (passthru_pst2 (res));
  CLEANSE; consume_pst2 (passthru_pst2_x0_asm (svptrue_b8 (), res));
  CLEANSE; consume_pst2 (passthru_pst2_x0 (svptrue_b8 (), res));
}

//--------------------------------------------------------------------------

struct __attribute__((packed, aligned (2))) pst3
{
  fixed_bool_t p;
  fixed_float16_t v;
};

ASM_FUNCTION (make_pst3_asm, struct pst3, (),
	      "ptrue p0.h, vl3\n\t"
	      "fmov z0.h, #5.0");

ASM_FUNCTION (passthru_pst3_asm, struct pst3, (struct pst3), "");

ASM_FUNCTION (passthru_pst3_p3_z7_asm,
	      struct pst3, (svbool_t, svbool_t, svbool_t,
			    svint32_t, svint32_t, svint32_t, svint32_t,
			    svint32_t, svint32_t, svint32_t, struct pst3),
	      "mov z0.d, z7.d\n\t"
	      "mov p0.b, p3.b");

ASM_FUNCTION (passthru_pst3_x0_asm,
	      struct pst3, (svbool_t, svbool_t, svbool_t, svbool_t,
			    struct pst3),
	      "addpl x1, x0, #1\n\t"
	      "ld1h z0.h, p1/z, [x1]\n\t"
	      "ldr p0, [x0]");

void
test_pst3 (struct pst3 *x)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, sveor_z (pg, x->p, svptrue_pat_b16 (SV_VL3)))
      || svptest_any (pg, svcmpne (pg, x->v, 5.0)))
    __builtin_abort ();
}

struct pst3 deref_pst3 (struct pst3 *ptr) { return *ptr; }
struct pst3 passthru_pst3 (struct pst3 x) { return x; }

struct pst3
passthru_pst3_p3_z7 (svbool_t p0, svbool_t p1, svbool_t p2,
		     svint32_t z0, svint32_t z1, svint32_t z2, svint32_t z3,
		     svint32_t z4, svint32_t z5, svint32_t z6,
		     struct pst3 p3_z7)
{
  return p3_z7;
}

struct pst3
passthru_pst3_x0 (svbool_t p0, svbool_t p1, svbool_t p2, svbool_t p3,
		  struct pst3 x0)
{
  return x0;
}

void consume_pst3 (struct pst3 x) { test_pst3 (&x); }

static void
run_pst3_tests (void)
{
  svint32_t s32 = svdup_s32 (0);
  svbool_t pg = svptrue_b8 ();

  CLEANSE; struct pst3 res = make_pst3_asm ();
  CLEANSE; test_pst3 (&res);
  CLEANSE; consume_pst3 (deref_pst3 (&res));
  CLEANSE; consume_pst3 (passthru_pst3_asm (res));
  CLEANSE; consume_pst3 (passthru_pst3 (res));
  CLEANSE; consume_pst3 (passthru_pst3_p3_z7_asm (pg, pg, pg,
						  s32, s32, s32, s32,
						  s32, s32, s32, res));
  CLEANSE; consume_pst3 (passthru_pst3_p3_z7 (pg, pg, pg,
					      s32, s32, s32, s32,
					      s32, s32, s32, res));
  CLEANSE; consume_pst3 (passthru_pst3_x0_asm (pg, pg, pg, pg, res));
  CLEANSE; consume_pst3 (passthru_pst3_x0 (pg, pg, pg, pg, res));
}

//--------------------------------------------------------------------------

struct pst4
{
  fixed_bool_t p1;
  fixed_bool_t p2 __attribute__((aligned (256)));
  fixed_bool_t p3 __attribute__((aligned (2048)));
};

ASM_FUNCTION (make_pst4_asm, struct pst4, (),
	      "ptrue p0.h, vl7\n\t"
	      "ptrue p1.h, mul3\n\t"
	      "ptrue p2.h, vl5");

ASM_FUNCTION (passthru_pst4_asm, struct pst4, (struct pst4), "");

ASM_FUNCTION (passthru_pst4_p1_asm,
	      struct pst4, (svbool_t, struct pst4),
	      "mov p0.b, p1.b\n\t"
	      "mov p1.b, p2.b\n\t"
	      "mov p2.b, p3.b");

ASM_FUNCTION (passthru_pst4_x0_asm,
	      struct pst4, (svbool_t, svbool_t, struct pst4),
	      "ldr p0, [x0]\n\t"
	      "add x0, x0, #256\n\t"
	      "ldr p1, [x0]\n\t"
	      "add x0, x0, #2048 - 256\n\t"
	      "ldr p2, [x0]");

void
test_pst4 (struct pst4 *x)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, sveor_z (pg, x->p1, svptrue_pat_b16 (SV_VL7)))
      || svptest_any (pg, sveor_z (pg, x->p2, svptrue_pat_b16 (SV_MUL3)))
      || svptest_any (pg, sveor_z (pg, x->p3, svptrue_pat_b16 (SV_VL5))))
    __builtin_abort ();
}

struct pst4 deref_pst4 (struct pst4 *ptr) { return *ptr; }
struct pst4 passthru_pst4 (struct pst4 x) { return x; }

struct pst4
passthru_pst4_p1 (svbool_t p0, struct pst4 p1)
{
  return p1;
}

struct pst4
passthru_pst4_x0 (svbool_t p0, svbool_t p1, struct pst4 x0)
{
  return x0;
}

void consume_pst4 (struct pst4 x) { test_pst4 (&x); }

static void
run_pst4_tests (void)
{
  svbool_t pg = svptrue_b8 ();

  CLEANSE; struct pst4 res = make_pst4_asm ();
  CLEANSE; test_pst4 (&res);
  CLEANSE; consume_pst4 (deref_pst4 (&res));
  CLEANSE; consume_pst4 (passthru_pst4_asm (res));
  CLEANSE; consume_pst4 (passthru_pst4 (res));
  CLEANSE; consume_pst4 (passthru_pst4_p1_asm (pg, res));
  CLEANSE; consume_pst4 (passthru_pst4_p1 (pg, res));
  CLEANSE; consume_pst4 (passthru_pst4_x0_asm (pg, pg, res));
  CLEANSE; consume_pst4 (passthru_pst4_x0 (pg, pg, res));
}

//--------------------------------------------------------------------------

struct pst5
{
  fixed_uint16_t v[8];
};

ASM_FUNCTION (make_pst5_asm, struct pst5, (),
	      "index z0.h, #0, #-1\n\t"
	      "index z1.h, #0, #-2\n\t"
	      "index z2.h, #0, #-3\n\t"
	      "index z3.h, #0, #-4\n\t"
	      "index z4.h, #0, #-5\n\t"
	      "index z5.h, #0, #-6\n\t"
	      "index z6.h, #0, #-7\n\t"
	      "index z7.h, #0, #-8");

ASM_FUNCTION (passthru_pst5_asm, struct pst5, (struct pst5), "");

void
test_pst5 (struct pst5 *x)
{
  svbool_t pg = svptrue_b8 ();
  for (int i = 0; i < 8; ++i)
    if (svptest_any (pg, svcmpne (pg, x->v[i], svindex_u16 (0, -1 - i))))
      __builtin_abort ();
}

struct pst5 deref_pst5 (struct pst5 *ptr) { return *ptr; }
struct pst5 passthru_pst5 (struct pst5 x) { return x; }

void consume_pst5 (struct pst5 x) { test_pst5 (&x); }

static void
run_pst5_tests (void)
{
  CLEANSE; struct pst5 res = make_pst5_asm ();
  CLEANSE; test_pst5 (&res);
  CLEANSE; consume_pst5 (deref_pst5 (&res));
  CLEANSE; consume_pst5 (passthru_pst5_asm (res));
  CLEANSE; consume_pst5 (passthru_pst5 (res));
}

//--------------------------------------------------------------------------

struct pst6
{
  fixed_uint16_t v[9];
};

ASM_FUNCTION (make_pst6_asm, struct pst6, (),
	      "mov x0, #10\n\t"
	      "ptrue p0.b\n"
	      "1:\n\t"
	      "index z0.h, #0, w0\n\t"
	      "st1h z0.h, p0, [x8]\n\t"
	      "add x0, x0, #1\n\t"
	      "incb x8\n\t"
	      "cmp x0, #19\n\t"
	      "bne 1b");

ASM_FUNCTION (passthru_pst6_asm, struct pst6, (struct pst6),
	      "mov x1, x0\n\t"
	      "mov x0, x8\n\t"
	      "cntb x2, all, mul #9\n\t"
	      "b memcpy");

void
test_pst6 (struct pst6 *x)
{
  svbool_t pg = svptrue_b8 ();
  for (int i = 0; i < 9; ++i)
    if (svptest_any (pg, svcmpne (pg, x->v[i], svindex_u16 (0, i + 10))))
      __builtin_abort ();
}

struct pst6 deref_pst6 (struct pst6 *ptr) { return *ptr; }
struct pst6 passthru_pst6 (struct pst6 x) { return x; }

void consume_pst6 (struct pst6 x) { test_pst6 (&x); }

static void
run_pst6_tests (void)
{
  CLEANSE; struct pst6 res = make_pst6_asm ();
  CLEANSE; test_pst6 (&res);
  CLEANSE; consume_pst6 (deref_pst6 (&res));
  CLEANSE; consume_pst6 (passthru_pst6_asm (res));
  CLEANSE; consume_pst6 (passthru_pst6 (res));
}

//--------------------------------------------------------------------------

struct pst7
{
  fixed_bool_t p[2][2];
};

ASM_FUNCTION (make_pst7_asm, struct pst7, (),
	      "ptrue p0.b, vl6\n\t"
	      "ptrue p1.b, vl7\n\t"
	      "ptrue p2.h, vl3\n\t"
	      "ptrue p3.h, vl2");

ASM_FUNCTION (passthru_pst7_asm, struct pst7, (struct pst7), "");

void
test_pst7 (struct pst7 *x)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, sveor_z (pg, x->p[0][0], svptrue_pat_b8 (SV_VL6)))
      || svptest_any (pg, sveor_z (pg, x->p[0][1], svptrue_pat_b8 (SV_VL7)))
      || svptest_any (pg, sveor_z (pg, x->p[1][0], svptrue_pat_b16 (SV_VL3)))
      || svptest_any (pg, sveor_z (pg, x->p[1][1], svptrue_pat_b16 (SV_VL2))))
    __builtin_abort ();
}

struct pst7 deref_pst7 (struct pst7 *ptr) { return *ptr; }
struct pst7 passthru_pst7 (struct pst7 x) { return x; }

void consume_pst7 (struct pst7 x) { test_pst7 (&x); }

static void
run_pst7_tests (void)
{
  CLEANSE; struct pst7 res = make_pst7_asm ();
  CLEANSE; test_pst7 (&res);
  CLEANSE; consume_pst7 (deref_pst7 (&res));
  CLEANSE; consume_pst7 (passthru_pst7_asm (res));
  CLEANSE; consume_pst7 (passthru_pst7 (res));
}

//--------------------------------------------------------------------------

struct pst8
{
  fixed_bool_t p[2][3];
};

ASM_FUNCTION (make_pst8_asm, struct pst8, (),
	      "ptrue p3.h, vl2\n\t"
	      "str p3, [x8]\n\t"
	      "ptrue p3.h, vl3\n\t"
	      "str p3, [x8, #1, mul vl]\n\t"
	      "ptrue p3.h, vl4\n\t"
	      "str p3, [x8, #2, mul vl]\n\t"
	      "ptrue p3.s, vl2\n\t"
	      "str p3, [x8, #3, mul vl]\n\t"
	      "ptrue p3.s, vl3\n\t"
	      "str p3, [x8, #4, mul vl]\n\t"
	      "ptrue p3.s, vl4\n\t"
	      "str p3, [x8, #5, mul vl]");

ASM_FUNCTION (passthru_pst8_asm, struct pst8, (struct pst8),
	      "cntw x1, all, mul #3\n\t"
	      "whilelo p0.b, xzr, x1\n\t"
	      "ld1b z0.b, p0/z, [x0]\n\t"
	      "st1b z0.b, p0, [x8]");

void
test_pst8 (struct pst8 *x)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, sveor_z (pg, x->p[0][0], svptrue_pat_b16 (SV_VL2)))
      || svptest_any (pg, sveor_z (pg, x->p[0][1], svptrue_pat_b16 (SV_VL3)))
      || svptest_any (pg, sveor_z (pg, x->p[0][2], svptrue_pat_b16 (SV_VL4)))
      || svptest_any (pg, sveor_z (pg, x->p[1][0], svptrue_pat_b32 (SV_VL2)))
      || svptest_any (pg, sveor_z (pg, x->p[1][1], svptrue_pat_b32 (SV_VL3)))
      || svptest_any (pg, sveor_z (pg, x->p[1][2], svptrue_pat_b32 (SV_VL4))))
    __builtin_abort ();
}

struct pst8 deref_pst8 (struct pst8 *ptr) { return *ptr; }
struct pst8 passthru_pst8 (struct pst8 x) { return x; }

void consume_pst8 (struct pst8 x) { test_pst8 (&x); }

static void
run_pst8_tests (void)
{
  CLEANSE; struct pst8 res = make_pst8_asm ();
  CLEANSE; test_pst8 (&res);
  CLEANSE; consume_pst8 (deref_pst8 (&res));
  CLEANSE; consume_pst8 (passthru_pst8_asm (res));
  CLEANSE; consume_pst8 (passthru_pst8 (res));
}

//--------------------------------------------------------------------------

struct nonpst1
{
  int x;
  fixed_uint8_t v;
  fixed_bool_t p;
};

ASM_FUNCTION (make_nonpst1_asm, struct nonpst1, (),
	      "mov w0, #42\n\t"
	      "str w0, [x8]\n\t"
	      "add x0, x8, #16\n\t"
	      "ptrue p0.b\n\t"
	      "index z0.b, #0, #3\n\t"
	      "st1b z0.b, p0, [x0]\n\t"
	      "ptrue p3.b, vl5\n\t"
	      "str p3, [x0, #8, mul vl]");

ASM_FUNCTION (passthru_nonpst1_asm, struct nonpst1, (struct nonpst1),
	      "mov x1, x0\n\t"
	      "mov x0, x8\n\t"
	      "cntd x2, all, mul #9\n\t"
	      "add x2, x2, #16\n\t"
	      "b memcpy");

void
test_nonpst1 (struct nonpst1 *x)
{
  svbool_t pg = svptrue_b8 ();
  if (x->x != 42
      || svptest_any (pg, svcmpne (pg, x->v, svindex_u8 (0, 3)))
      || svptest_any (pg, sveor_z (pg, x->p, svptrue_pat_b8 (SV_VL5))))
    __builtin_abort ();
}

struct nonpst1 deref_nonpst1 (struct nonpst1 *ptr) { return *ptr; }
struct nonpst1 passthru_nonpst1 (struct nonpst1 x) { return x; }

void consume_nonpst1 (struct nonpst1 x) { test_nonpst1 (&x); }

static void
run_nonpst1_tests (void)
{
  CLEANSE; struct nonpst1 res = make_nonpst1_asm ();
  CLEANSE; test_nonpst1 (&res);
  CLEANSE; consume_nonpst1 (deref_nonpst1 (&res));
  CLEANSE; consume_nonpst1 (passthru_nonpst1_asm (res));
  CLEANSE; consume_nonpst1 (passthru_nonpst1 (res));
}

//--------------------------------------------------------------------------

struct nonpst2
{
  union { struct { fixed_bool_t p; }; };
};

ASM_FUNCTION (make_nonpst2_asm, struct nonpst2, (),
	      "ptrue p3.h, mul3\n\t"
	      "cntd x2\n\t"
	      "cmp x2, #16\n\t"
	      "b.ls 1f\n\t"
	      "str p3, [x8]\n\t"
	      "ret\n"
	      "1:\n\t"
	      "addvl sp, sp, #-1\n\t"
	      "str p3, [sp]\n\t"
	      "ldp x0, x1, [sp]\n\t"
	      "addvl sp, sp, #1");

ASM_FUNCTION (passthru_nonpst2_asm, struct nonpst2, (struct nonpst2),
	      "cntb x2\n\t"
	      "cmp x2, #128\n\t"
	      "b.eq 1f\n\t"
	      "b.lo 2f\n\t"
	      "ldr p3, [x0]\n\t"
	      "str p3, [x8]\n"
	      "1:\n\t"
	      "ret\n"
	      "2:\n\t"
	      "mov x3, #-1\n\t"
#if __ARM_BIG_ENDIAN
	      "lsr x3, x3, x2\n\t"
#else
	      "lsl x3, x3, x2\n\t"
#endif
	      "bic x1, x0, x3\n\t"
	      "cmp x2, #64\n\t"
	      "csel x0, x0, x1, eq");

void
test_nonpst2 (struct nonpst2 *x)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, sveor_z (pg, x->p, svptrue_pat_b16 (SV_MUL3))))
    __builtin_abort ();
}

struct nonpst2 deref_nonpst2 (struct nonpst2 *ptr) { return *ptr; }
struct nonpst2 passthru_nonpst2 (struct nonpst2 x) { return x; }

void consume_nonpst2 (struct nonpst2 x) { test_nonpst2 (&x); }

static void
run_nonpst2_tests (void)
{
  CLEANSE; struct nonpst2 res = make_nonpst2_asm ();
  CLEANSE; test_nonpst2 (&res);
  CLEANSE; consume_nonpst2 (deref_nonpst2 (&res));
  CLEANSE; consume_nonpst2 (passthru_nonpst2_asm (res));
  CLEANSE; consume_nonpst2 (passthru_nonpst2 (res));
}

//--------------------------------------------------------------------------

struct nonpst3
{
  union { struct { fixed_int32_t v; }; };
};

ASM_FUNCTION (make_nonpst3_asm, struct nonpst3, (),
	      "ptrue p0.b\n\t"
	      "index z1.s, #15, #-9\n\t"
	      "cntb x2\n\t"
	      "cmp x2, #16\n\t"
	      "b.ls 1f\n\t"
	      "st1w z1.s, p0, [x8]\n\t"
	      "ret\n"
	      "1:\n\t"
	      "addvl sp, sp, #-1\n\t"
	      "st1w z1.s, p0, [sp]\n\t"
	      "ldp x0, x1, [sp]\n\t"
	      "addvl sp, sp, #1");

ASM_FUNCTION (passthru_nonpst3_asm, struct nonpst3, (struct nonpst3),
	      "cntb x2\n\t"
	      "cmp x2, #16\n\t"
	      "b.ls 1f\n\t"
	      "ptrue p0.b\n\t"
	      "ld1w z1.s, p0/z, [x0]\n\t"
	      "st1w z1.s, p0, [x8]\n"
	      "1:");

void
test_nonpst3 (struct nonpst3 *x)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, svcmpne (pg, x->v, svindex_s32 (15, -9))))
    __builtin_abort ();
}

struct nonpst3 deref_nonpst3 (struct nonpst3 *ptr) { return *ptr; }
struct nonpst3 passthru_nonpst3 (struct nonpst3 x) { return x; }

void consume_nonpst3 (struct nonpst3 x) { test_nonpst3 (&x); }

static void
run_nonpst3_tests (void)
{
  CLEANSE; struct nonpst3 res = make_nonpst3_asm ();
  CLEANSE; test_nonpst3 (&res);
  CLEANSE; consume_nonpst3 (deref_nonpst3 (&res));
  CLEANSE; consume_nonpst3 (passthru_nonpst3_asm (res));
  CLEANSE; consume_nonpst3 (passthru_nonpst3 (res));
}

//--------------------------------------------------------------------------

int
main (void)
{
  run_pst1_tests ();
  run_pst2_tests ();
  run_pst3_tests ();
  run_pst4_tests ();
  run_pst5_tests ();
  run_pst6_tests ();
  run_pst7_tests ();
  run_pst8_tests ();
  run_nonpst1_tests ();
  run_nonpst2_tests ();
  run_nonpst3_tests ();
  return 0;
}
