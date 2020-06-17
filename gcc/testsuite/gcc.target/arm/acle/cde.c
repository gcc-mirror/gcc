/* { dg-do compile } */
/* { dg-skip-if "Require optimsation to compile DCE tests" { *-*-* } { "-O0" } { "" } } */
/* { dg-require-effective-target arm_v8m_main_cde_ok } */
/* { dg-add-options arm_v8m_main_cde } */
/* { dg-final { check-function-bodies "**" "" } } */

/* These are the scalar intrinsics.
uint32_t __arm_cx1(int coproc, uint32_t imm);
uint32_t __arm_cx1a(int coproc, uint32_t acc, uint32_t imm);
uint32_t __arm_cx2(int coproc, uint32_t n, uint32_t imm);
uint32_t __arm_cx2a(int coproc, uint32_t acc, uint32_t n, uint32_t imm);
uint32_t __arm_cx3(int coproc, uint32_t n, uint32_t m, uint32_t imm);
uint32_t __arm_cx3a(int coproc, uint32_t acc, uint32_t n, uint32_t m, uint32_t imm);

uint64_t __arm_cx1d(int coproc, uint32_t imm);
uint64_t __arm_cx1da(int coproc, uint64_t acc, uint32_t imm);
uint64_t __arm_cx2d(int coproc, uint32_t n, uint32_t imm);
uint64_t __arm_cx2da(int coproc, uint64_t acc, uint32_t n, uint32_t imm);
uint64_t __arm_cx3d(int coproc, uint32_t n, uint32_t m, uint32_t imm);
uint64_t __arm_cx3da(int coproc, uint64_t acc, uint32_t n, uint32_t m, uint32_t imm); */

#include "arm_cde.h"

#define TEST_CDE_SCALAR_INTRINSIC(name, accum_type, arguments) \
  accum_type test_cde_##name (__attribute__ ((unused)) uint32_t n, \
			      __attribute__ ((unused)) uint32_t m) \
  {  \
     accum_type accum = 0; \
     accum += __arm_##name  arguments;  \
     return accum;  \
  }

/* Basic test that we produce the assembly as expected.  */
/*
** test_cde_cx1:
**	cx1	p0, r0, #33
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx1, uint32_t, (0, 33))

/*
** test_cde_cx1a:
**	movs	r0, #0
**	cx1a	p0, r0, #33
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx1a, uint32_t, (0, accum, 33))

/*
** test_cde_cx2:
**	cx2	p0, r0, r0, #33
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx2, uint32_t, (0, n, 33))

/*
** test_cde_cx2a:
**	movs	(r[0-9]+), #0
**	cx2a	p0, \1, r0, #33
**	mov	r0, \1
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx2a, uint32_t, (0, accum, n, 33))

/*
** test_cde_cx3:
**	cx3	p0, r0, r0, r1, #33
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx3, uint32_t, (0, n, m, 33))

/*
** test_cde_cx3a:
**	movs	(r[0-9]+), #0
**	cx3a	p0, \1, r0, r1, #33
**	mov	r0, \1
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx3a, uint32_t, (0, accum, n, m, 33))

/*
** test_cde_cx1d:
**	cx1d	p0, r0, r1, #33
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx1d, uint64_t, (0, 33))

/*
** test_cde_cx1da:
**	movs	r0, #0
**	movs	r1, #0
**	cx1da	p0, r0, r1, #33
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx1da, uint64_t, (0, accum, 33))

/*
** test_cde_cx2d:
**	cx2d	p0, r0, r1, r0, #33
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx2d, uint64_t, (0, n, 33))

/* This particular function gets optimised by the compiler in two different
   ways depending on the optimisation level.  So does test_cde_cx3da.  That's
   why we have two different regexes in each of these function body checks.  */
/*
** test_cde_cx2da:
** (
**	mov	(r[0-9]+), r0
**	movs	r0, #0
**	movs	r1, #0
**	cx2da	p0, r0, r1, \1, #33
** |
**	movs	(r[0-9]+), #0
**	movs	(r[0-9]+), #0
**	cx2da	p0, \2, \3, r0, #33
**	mov	r0, \2
**	mov	r1, \3
** )
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx2da, uint64_t, (0, accum, n, 33))

/*
** test_cde_cx3d:
**	cx3d	p0, r0, r1, r0, r1, #33
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx3d, uint64_t, (0, n, m, 33))

/*
** test_cde_cx3da:
**	...
** (
**	movs	(r[0-9]+), #0
**	movs	(r[0-9]+), #0
**	cx3da	p0, \1, \2, r0, r1, #33
**	mov	r0, \1
**	mov	r1, \2
** |
**      movs	r0, #0
**      movs	r1, #0
**      cx3da	p0, r0, r1, r[0-9]+, r[0-9]+, #33
** )
**	...
**	bx	lr
*/
TEST_CDE_SCALAR_INTRINSIC (cx3da, uint64_t, (0, accum, n, m, 33))



/* Ensure this function gets DCE'd out after optimisation.
   Should be such since the ACLE specification mentions these functions are
   stateless and pure.  */
/*
** test_cde_dce:
**	bx	lr
*/
void test_cde_dce (uint32_t n, uint32_t m)
{
  uint64_t accum = 0;
  __arm_cx1   (0, 33);
  __arm_cx1a  (0, accum, 33);
  __arm_cx2   (0, n, 33);
  __arm_cx2a  (0, accum, n, 33);
  __arm_cx3   (0, n, m, 33);
  __arm_cx3a  (0, accum, n, m, 33);
  __arm_cx1d   (0, 33);
  __arm_cx1da  (0, accum, 33);
  __arm_cx2d   (0, n, 33);
  __arm_cx2da  (0, accum, n, 33);
  __arm_cx3d   (0, n, m, 33);
  __arm_cx3da  (0, accum, n, m, 33);
}

/* Checking this function allows constants with symbolic names.
   This test must be run under some level of optimisation.
   The actual check we perform is that the function is provided something that,
   at the point of expansion, is an immediate.  That check is not as strict as
   having something that is an immediate directly.

   Since we've already checked these intrinsics generate code in the manner we
   expect (above), here we just check that all the instructions we expect are
   there.  To ensure the instructions are from these functions we use different
   constants and search for those specifically with `scan-assembler-times`.  */

/* Checking this function allows constants with symbolic names.  */
uint32_t test_cde2 (uint32_t n, uint32_t m)
{
  int coproc = 6;
  uint32_t imm = 30;
  uint32_t accum = 0;
  accum += __arm_cx1   (coproc, imm);
  accum += __arm_cx1a  (coproc, accum, imm);
  accum += __arm_cx2   (coproc, n, imm);
  accum += __arm_cx2a  (coproc, accum, n, imm);
  accum += __arm_cx3   (coproc, n, m, imm);
  accum += __arm_cx3a  (coproc, accum, n, m, imm);
  return accum;
}

/* Checking this function allows constants with symbolic names.  */
uint64_t test_cdedi2 (uint32_t n, uint32_t m)
{
  int coproc = 6;
  uint32_t imm = 30;
  uint64_t accum = 0;
  accum += __arm_cx1d   (coproc, imm);
  accum += __arm_cx1da  (coproc, accum, imm);
  accum += __arm_cx2d   (coproc, n, imm);
  accum += __arm_cx2da  (coproc, accum, n, imm);
  accum += __arm_cx3d   (coproc, n, m, imm);
  accum += __arm_cx3da  (coproc, accum, n, m, imm);
  return accum;
}

/* { dg-final { scan-assembler-times "cx1\\tp6" 1 } } */
/* { dg-final { scan-assembler-times "cx2\\tp6" 1 } } */
/* { dg-final { scan-assembler-times "cx3\\tp6" 1 } } */
/* { dg-final { scan-assembler-times "cx1a\\tp6" 1 } } */
/* { dg-final { scan-assembler-times "cx2a\\tp6" 1 } } */
/* { dg-final { scan-assembler-times "cx3a\\tp6" 1 } } */
/* { dg-final { scan-assembler-times "cx1d\\tp6" 1 } } */
/* { dg-final { scan-assembler-times "cx2d\\tp6" 1 } } */
/* { dg-final { scan-assembler-times "cx3d\\tp6" 1 } } */
/* { dg-final { scan-assembler-times "cx1da\\tp6" 1 } } */
/* { dg-final { scan-assembler-times "cx2da\\tp6" 1 } } */
/* { dg-final { scan-assembler-times "cx3da\\tp6" 1 } } */
