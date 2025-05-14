/* { dg-do compile } */
/* { dg-options "-msve-vector-bits=256 -fopenmp -O2" } */

#include <arm_sve.h>

#define N __ARM_FEATURE_SVE_BITS
#define FIXED_ATTR __attribute__((arm_sve_vector_bits (N)))

typedef __SVInt32_t v8si FIXED_ATTR;

static v8si local_vec;
#pragma omp declare target link(local_vec)

v8si global_vec;
#pragma omp declare target link(global_vec)

/* { dg-error {SVE type 'svint32_t' does not have a fixed size} "" { target *-*-* } .+1 } */
static svint32_t slocal_vec;

/* { dg-error {'slocal_vec' does not have a mappable type in 'link' clause} "" { target *-*-* } .+1 } */
#pragma omp declare target link(slocal_vec)

void
one_get_inc2_local_vec_vls ()
{
  v8si res, res2, tmp;

#pragma omp target map(from: res, res2)
  {
    res = local_vec;
    local_vec = svadd_s32_z (svptrue_b32 (), local_vec, local_vec);
    res2 = local_vec;
  }

  tmp = svadd_s32_z (svptrue_b32 (), res, res);
  svbool_t p = svcmpne_s32 (svptrue_b32 (), tmp, res2);
  if (svptest_any (svptrue_b32 (), p))
    __builtin_abort ();
}

void
one_get_inc3_global_vec_vls ()
{
  v8si res, res2, tmp;

#pragma omp target map(from: res, res2)
  {
    res = global_vec;
    global_vec = svadd_s32_z (svptrue_b32 (), global_vec, global_vec);
    res2 = global_vec;
  }

  tmp = svadd_s32_z (svptrue_b32 (), res, res);
  svbool_t p = svcmpne_s32 (svptrue_b32 (), tmp, res2);
  if (svptest_any (svptrue_b32 (), p))
    __builtin_abort ();
}
