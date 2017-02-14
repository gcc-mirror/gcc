/* { dg-do run } */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_hw } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define FP16_C(a) ((__fp16) a)
#define A0 FP16_C (34.8)
#define B0 FP16_C (__builtin_nanf (""))
#define C0 FP16_C (-__builtin_nanf (""))
#define D0 FP16_C (0.0)

#define A1 FP16_C (1025.8)
#define B1 FP16_C (13.4)
#define C1 FP16_C (__builtin_nanf (""))
#define D1 FP16_C (10)
#define E1 FP16_C (-0.0)
#define F1 FP16_C (-__builtin_nanf (""))
#define G1 FP16_C (0.0)
#define H1 FP16_C (10)

/* Expected results for vmaxnmv.  */
uint16_t expect = 0x505A /* A0.  */;
uint16_t expect_alt = 0x6402 /* A1.  */;

void exec_vmaxnmv_f16 (void)
{
#undef TEST_MSG
#define TEST_MSG "VMAXNMV (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc, float, 16, 4);
  VECT_VAR_DECL (buf_src, float, 16, 4) [] = {A0, B0, C0, D0};
  VLOAD (vsrc, buf_src, , float, f, 16, 4);
  float16_t vector_res = vmaxnmv_f16 (VECT_VAR (vsrc, float, 16, 4));

  if (* (uint16_t *) &vector_res != expect)
    abort ();

  VECT_VAR_DECL (buf_src1, float, 16, 4) [] = {B0, A0, C0, D0};
  VLOAD (vsrc, buf_src1, , float, f, 16, 4);
  vector_res = vmaxnmv_f16 (VECT_VAR (vsrc, float, 16, 4));

  if (* (uint16_t *) &vector_res != expect)
    abort ();

  VECT_VAR_DECL (buf_src2, float, 16, 4) [] = {B0, C0, A0, D0};
  VLOAD (vsrc, buf_src2, , float, f, 16, 4);
  vector_res = vmaxnmv_f16 (VECT_VAR (vsrc, float, 16, 4));

  if (* (uint16_t *) &vector_res != expect)
    abort ();

  VECT_VAR_DECL (buf_src3, float, 16, 4) [] = {B0, C0, D0, A0};
  VLOAD (vsrc, buf_src3, , float, f, 16, 4);
  vector_res = vmaxnmv_f16 (VECT_VAR (vsrc, float, 16, 4));

  if (* (uint16_t *) &vector_res != expect)
    abort ();

#undef TEST_MSG
#define TEST_MSG "VMAXNMVQ (FP16)"
  clean_results ();

  DECL_VARIABLE(vsrc, float, 16, 8);
  VECT_VAR_DECL (buf_src, float, 16, 8) [] = {A1, B1, C1, D1, E1, F1, G1, H1};
  VLOAD (vsrc, buf_src, q, float, f, 16, 8);
  vector_res = vmaxnmvq_f16 (VECT_VAR (vsrc, float, 16, 8));

  if (* (uint16_t *) &vector_res != expect_alt)
    abort ();

  VECT_VAR_DECL (buf_src1, float, 16, 8) [] = {B1, A1, C1, D1, E1, F1, G1, H1};
  VLOAD (vsrc, buf_src1, q, float, f, 16, 8);
  vector_res = vmaxnmvq_f16 (VECT_VAR (vsrc, float, 16, 8));

  if (* (uint16_t *) &vector_res != expect_alt)
    abort ();

  VECT_VAR_DECL (buf_src2, float, 16, 8) [] = {B1, C1, A1, D1, E1, F1, G1, H1};
  VLOAD (vsrc, buf_src2, q, float, f, 16, 8);
  vector_res = vmaxnmvq_f16 (VECT_VAR (vsrc, float, 16, 8));

  if (* (uint16_t *) &vector_res != expect_alt)
    abort ();

  VECT_VAR_DECL (buf_src3, float, 16, 8) [] = {B1, C1, D1, A1, E1, F1, G1, H1};
  VLOAD (vsrc, buf_src3, q, float, f, 16, 8);
  vector_res = vmaxnmvq_f16 (VECT_VAR (vsrc, float, 16, 8));

  if (* (uint16_t *) &vector_res != expect_alt)
    abort ();

  VECT_VAR_DECL (buf_src4, float, 16, 8) [] = {B1, C1, D1, E1, A1, F1, G1, H1};
  VLOAD (vsrc, buf_src4, q, float, f, 16, 8);
  vector_res = vmaxnmvq_f16 (VECT_VAR (vsrc, float, 16, 8));

  if (* (uint16_t *) &vector_res != expect_alt)
    abort ();

  VECT_VAR_DECL (buf_src5, float, 16, 8) [] = {B1, C1, D1, E1, F1, A1, G1, H1};
  VLOAD (vsrc, buf_src5, q, float, f, 16, 8);
  vector_res = vmaxnmvq_f16 (VECT_VAR (vsrc, float, 16, 8));

  if (* (uint16_t *) &vector_res != expect_alt)
    abort ();

  VECT_VAR_DECL (buf_src6, float, 16, 8) [] = {B1, C1, D1, E1, F1, G1, A1, H1};
  VLOAD (vsrc, buf_src6, q, float, f, 16, 8);
  vector_res = vmaxnmvq_f16 (VECT_VAR (vsrc, float, 16, 8));

  if (* (uint16_t *) &vector_res != expect_alt)
    abort ();

  VECT_VAR_DECL (buf_src7, float, 16, 8) [] = {B1, C1, D1, E1, F1, G1, H1, A1};
  VLOAD (vsrc, buf_src7, q, float, f, 16, 8);
  vector_res = vmaxnmvq_f16 (VECT_VAR (vsrc, float, 16, 8));

  if (* (uint16_t *) &vector_res != expect_alt)
    abort ();
}

int
main (void)
{
  exec_vmaxnmv_f16 ();
  return 0;
}
