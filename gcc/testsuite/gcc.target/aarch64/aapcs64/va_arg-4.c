/* Test AAPCS64 layout and __builtin_va_arg.

   This test covers homogeneous floating-point aggregate types and homogeneous
   short-vector aggregate types as described in AAPCS64 \S 4.3.5.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-4.c"
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
struct hva_vf2x2_t hva_vf2x2;
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
  hva_vf2x2.a = (vf2_t){19.f, 20.f};
  hva_vf2x2.b = (vf2_t){21.f, 22.f};
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

#include "abitest.h"
#else
  ARG      (int   , 1, X0, LAST_NAMED_ARG_ID)
  DOTS
  /* HFA or HVA passed in fp/simd registers or on stack.  */
  ANON     (struct hfa_fx1_t  , hfa_fx1 ,  S0      , 0)
  ANON     (struct hfa_fx2_t  , hfa_fx2 ,  S1      , 1)
  ANON     (struct hfa_dx2_t  , hfa_dx2 ,  D3      , 2)
  ANON     (struct hva_vf2x1_t, hva_vf2x1, D5      , 11)
  ANON     (struct hva_vi4x1_t, hva_vi4x1, Q6      , 12)
  ANON     (struct hfa_dx4_t  , hfa_dx4 ,  STACK   , 3)
  ANON     (struct hfa_ffs_t  , hfa_ffs  , STACK+32, 4)
  ANON     (union  hfa_union_t, hfa_union, STACK+48, 5)
  ANON     (struct hfa_ldx3_t , hfa_ldx3 , STACK+64, 6)
  /* Non-H[FV]A passed in general registers or on stack or via reference.  */
  PTR_ANON (struct non_hfa_fx5_t  , non_hfa_fx5  , X1       , 10)
  ANON     (struct non_hfa_ffd_t  , non_hfa_ffd  , X2       , 13)
  ANON     (struct non_hfa_ii_t   , non_hfa_ii   , X4       , 14)
  ANON     (struct non_hfa_c_t    , non_hfa_c    , X5       , 15)
  ANON     (struct non_hfa_ffvf2_t, non_hfa_ffvf2, X6       , 16)
  PTR_ANON (struct non_hfa_fffd_t , non_hfa_fffd , STACK+112, 17)
  PTR_ANON (struct non_hfa_ffs_t  , non_hfa_ffs  , STACK+120, 18)
  ANON     (struct non_hfa_ffs_2_t, non_hfa_ffs_2, STACK+128, 19)
  ANON     (union  non_hfa_union_t, non_hfa_union, STACK+144, 20)
  LAST_ANON(int                   , 2            , STACK+152, 30)
#endif
