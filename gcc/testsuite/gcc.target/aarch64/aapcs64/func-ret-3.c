/* Test AAPCS64 function result return.

   This test covers homogeneous floating-point aggregate types as described
   in AAPCS64 \S 4.3.5.  */

/* { dg-do run { target aarch64-*-* } } */
/* { dg-additional-sources "abitest.S" } */
/* { dg-require-effective-target aarch64_big_endian } */

#ifndef IN_FRAMEWORK
#define TESTFILE "func-ret-3.c"
#include "type-def.h"

struct hfa_fx1_t hfa_fx1 = {12.345f};
struct hfa_fx2_t hfa_fx2 = {123.456f, 234.456f};
struct hfa_dx2_t hfa_dx2 = {234.567, 345.678};
struct hfa_dx4_t hfa_dx4 = {1234.123, 2345.234, 3456.345, 4567.456};
struct hfa_ldx3_t hfa_ldx3 = {123456.7890, 234567.8901, 345678.9012};
struct non_hfa_fx5_t non_hfa_fx5 = {456.789f, 567.890f, 678.901f, 789.012f, 890.123f};
struct hfa_ffs_t hfa_ffs;
struct non_hfa_ffs_t non_hfa_ffs;
struct non_hfa_ffs_2_t non_hfa_ffs_2;
struct hva_vf2x1_t hva_vf2x1;
struct hva_vi4x1_t hva_vi4x1;
struct non_hfa_ffd_t non_hfa_ffd = {23.f, 24.f, 25.0};
struct non_hfa_ii_t non_hfa_ii = {26, 27};
struct non_hfa_c_t non_hfa_c = {28};
struct non_hfa_ffvf2_t non_hfa_ffvf2;
struct non_hfa_fffd_t non_hfa_fffd = {33.f, 34.f, 35.f, 36.0};
union hfa_union_t hfa_union;
union non_hfa_union_t non_hfa_union;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  hva_vf2x1.a = (vf2_t){17.f, 18.f};
  hva_vi4x1.a = (vi4_t){19, 20, 21, 22};

  non_hfa_ffvf2.a = 29.f;
  non_hfa_ffvf2.b = 30.f;
  non_hfa_ffvf2.c = (vf2_t){31.f, 32.f};

  hfa_union.s.a = 37.f;
  hfa_union.s.b = 38.f;
  hfa_union.c   = 39.f;

  non_hfa_union.a = 40.0;
  non_hfa_union.b = 41.f;

  hfa_ffs.a = 42.f;
  hfa_ffs.b = 43.f;
  hfa_ffs.c.a = 44.f;
  hfa_ffs.c.b = 45.f;

  non_hfa_ffs.a = 46.f;
  non_hfa_ffs.b = 47.f;
  non_hfa_ffs.c.a = 48.0;
  non_hfa_ffs.c.b = 49.0;

  non_hfa_ffs_2.s.a = 50;
  non_hfa_ffs_2.s.b = 51;
  non_hfa_ffs_2.c = 52.f;
  non_hfa_ffs_2.d = 53.f;
}

#include "abitest-2.h"
#else
  /* HFA returned in fp/simd registers.  */

FUNC_VAL_CHECK ( 0, struct hfa_fx1_t , hfa_fx1  , S0, flat)
FUNC_VAL_CHECK ( 1, struct hfa_fx2_t , hfa_fx2  , S0, flat)
FUNC_VAL_CHECK ( 2, struct hfa_dx2_t , hfa_dx2  , D0, flat)

FUNC_VAL_CHECK ( 3, struct hfa_dx4_t , hfa_dx4  , D0, flat)
FUNC_VAL_CHECK ( 4, struct hfa_ldx3_t, hfa_ldx3 , Q0, flat)
FUNC_VAL_CHECK ( 5, struct hfa_ffs_t , hfa_ffs  , S0, flat)
FUNC_VAL_CHECK ( 6, union hfa_union_t, hfa_union, S0, flat)

FUNC_VAL_CHECK ( 7, struct hva_vf2x1_t, hva_vf2x1, D0, flat)
FUNC_VAL_CHECK ( 8, struct hva_vi4x1_t, hva_vi4x1, Q0, flat)

  /* Non-HFA returned in general registers or via a pointer in X8.  */
FUNC_VAL_CHECK (10, struct non_hfa_fx5_t  , non_hfa_fx5  , X8, flat)
FUNC_VAL_CHECK (13, struct non_hfa_ffd_t  , non_hfa_ffd  , X0, flat)
FUNC_VAL_CHECK (14, struct non_hfa_ii_t   , non_hfa_ii   , X0, flat)
FUNC_VAL_CHECK (15, struct non_hfa_c_t    , non_hfa_c    , X0, flat)
FUNC_VAL_CHECK (16, struct non_hfa_ffvf2_t, non_hfa_ffvf2, X0, flat)
FUNC_VAL_CHECK (17, struct non_hfa_fffd_t , non_hfa_fffd , X8, flat)
FUNC_VAL_CHECK (18, struct non_hfa_ffs_t  , non_hfa_ffs  , X8, flat)
FUNC_VAL_CHECK (19, struct non_hfa_ffs_2_t, non_hfa_ffs_2, X0, flat)
FUNC_VAL_CHECK (20, union  non_hfa_union_t, non_hfa_union, X0, flat)

#endif
