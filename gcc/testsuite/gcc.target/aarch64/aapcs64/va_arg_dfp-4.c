/* Test AAPCS64 layout and __builtin_va_arg.

   This test covers homogeneous floating-point aggregate types and homogeneous
   short-vector aggregate types as described in AAPCS64 \S 4.3.5.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-4.c"
#include "type-def.h"

struct hfa_dfx1_t hfa_dfx1 = {12.345df};
struct hfa_dfx2_t hfa_dfx2 = {123.456df, 234.456df};
struct hfa_ddx2_t hfa_ddx2 = {234.567dd, 345.678dd};
struct hfa_ddx4_t hfa_ddx4 = {1234.123dd, 2345.234dd, 3456.345dd, 4567.456dd};
struct hfa_dldx3_t hfa_dldx3 = {123456.7890dl, 234567.8901dl, 345678.9012dl};
struct non_hfa_dfx5_t non_hfa_dfx5 = {456.789df, 567.890df, 678.901df, 789.012df, 890.123df};
struct hfa_dffs_t hfa_dffs;
struct non_hfa_dffs_t non_hfa_dffs;
struct non_hfa_dffs_2_t non_hfa_dffs_2;
struct hva_vdf2x1_t hva_vdf2x1;
struct hva_vdf2x2_t hva_vdf2x2;
struct non_hfa_dffd_t non_hfa_dffd = {23.df, 24.df, 25.0dd};
struct non_hfa_dffvf2_t non_hfa_dffvf2;
struct non_hfa_dfffd_t non_hfa_dfffd = {33.df, 34.df, 35.df, 36.0dd};
union hfa_dunion_t hfa_dunion;
union non_hfa_union_t non_hfa_union;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  hva_vdf2x1.a = (vdf2_t){17.df, 18.df};
  hva_vdf2x2.a = (vdf2_t){19.df, 20.df};
  hva_vdf2x2.b = (vdf2_t){21.df, 22.df};

  non_hfa_dffvf2.a = 29.df;
  non_hfa_dffvf2.b = 30.df;
  non_hfa_dffvf2.c = (vdf2_t){31.df, 32.df};

  hfa_dunion.s.a = 37.df;
  hfa_dunion.s.b = 38.df;
  hfa_dunion.c   = 39.df;

  non_hfa_dunion.a = 40.0dd;
  non_hfa_dunion.b = 41.df;

  hfa_dffs.a = 42.df;
  hfa_dffs.b = 43.df;
  hfa_dffs.c.a = 44.df;
  hfa_dffs.c.b = 45.df;

  non_hfa_dffs.a = 46.df;
  non_hfa_dffs.b = 47.df;
  non_hfa_dffs.c.a = 48.0dd;
  non_hfa_dffs.c.b = 49.0dd;

  non_hfa_dffs_2.s.a = 50;
  non_hfa_dffs_2.s.b = 51;
  non_hfa_dffs_2.c = 52.df;
  non_hfa_dffs_2.d = 53.df;
}

#include "abitest.h"
#else
  ARG      (int   , 1, W0, LAST_NAMED_ARG_ID)
  DOTS
  /* HFA or HVA passed in fp/simd registers or on stack.  */
  ANON     (struct hfa_dfx1_t , hfa_dfx1,  S0      , 0)
  ANON     (struct hfa_dfx2_t , hfa_dfx2,  S1      , 1)
  ANON     (struct hfa_ddx2_t , hfa_ddx2,  D3      , 2)
  ANON     (struct hva_vdf2x1_t, hva_vdf2x1, D5      , 11)
  ANON     (struct hfa_ddx4_t , hfa_ddx4,  STACK   , 3)
  ANON     (struct hfa_dffs_t , hfa_dffs , STACK+32, 4)
  ANON     (union  hfa_dunion_t, hfa_dunion, STACK+48, 5)
  ANON     (struct hfa_dldx3_t, hfa_dldx3, STACK+64, 6)
  /* Non-H[FV]A passed in general registers or on stack or via reference.  */
  PTR_ANON (struct non_hfa_dfx5_t , non_hfa_dfx5 , X1       , 10)
  ANON     (struct non_hfa_dffd_t , non_hfa_dffd , X2       , 13)
  ANON     (struct non_hfa_dffvf2_t, non_hfa_dffvf2, X6       , 16)
  PTR_ANON (struct non_hfa_dfffd_t, non_hfa_dfffd, STACK+112, 17)
  PTR_ANON (struct non_hfa_dffs_t , non_hfa_dffs , STACK+120, 18)
  ANON     (struct non_hfa_dffs_2_t, non_hfa_dffs_2, STACK+128, 19)
  ANON     (union  non_hfa_dunion_t, non_hfa_dunion, STACK+144, 20)
#ifndef __AAPCS64_BIG_ENDIAN__
  LAST_ANON(int                   , 2            , STACK+152, 30)
#else
  LAST_ANON(int                   , 2            , STACK+156, 30)
#endif
#endif
