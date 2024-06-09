/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include "riscv_rvv_vector_bits.h"

DEF_FIXED_TYPE (vint32m1_t, 256)
DEF_FIXED_TYPE (vfloat32m1_t, 256)
DEF_FIXED_TYPE (vbool1_t, 256)
DEF_FIXED_TYPE (vbool2_t, 128)

fixed_vint32m1_t
test_1 (fixed_vint32m1_t a, fixed_vint32m1_t b)
{
  return a == b;
}

fixed_vbool1_t
test_2 (fixed_vint32m1_t a, fixed_vint32m1_t b)
{
  return a == b;
}

fixed_vfloat32m1_t
test_3 (fixed_vfloat32m1_t a, fixed_vfloat32m1_t b)
{
  return a == b;
}

fixed_vbool1_t
test_4 (fixed_vfloat32m1_t a, fixed_vfloat32m1_t b)
{
  return a == b;
}

fixed_vbool2_t
test_5 (fixed_vint32m1_t a, fixed_vint32m1_t b)
{
  return a == b; /* { dg-error {incompatible types when returning type '__vector\(8\) int' but 'fixed_vbool2_t' {aka 'vbool2_t'} was expected} } */
}

fixed_vbool2_t
test_6 (fixed_vfloat32m1_t a, fixed_vfloat32m1_t b)
{
  return a == b; /* { dg-error {incompatible types when returning type '__vector\(8\) int' but 'fixed_vbool2_t' {aka 'vbool2_t'} was expected} } */
}
