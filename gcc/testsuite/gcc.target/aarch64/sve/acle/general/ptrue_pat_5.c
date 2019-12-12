/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

void
b8_b16_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b8 (SV_VL64);
  *any = svptest_any (svptrue_b16 (), res);
  *ptr = res;
}

int
b8_b16_2 ()
{
  svbool_t res = svptrue_pat_b8 (SV_VL64);
  return svptest_any (svptrue_b16 (), res);
}

void
b8_b32_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b8 (SV_VL32);
  *any = svptest_any (svptrue_b32 (), res);
  *ptr = res;
}

int
b8_b32_2 ()
{
  svbool_t res = svptrue_pat_b8 (SV_VL32);
  return svptest_any (svptrue_b32 (), res);
}

void
b8_b64_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b8 (SV_VL128);
  *any = svptest_any (svptrue_b64 (), res);
  *ptr = res;
}

int
b8_b64_2 ()
{
  svbool_t res = svptrue_pat_b8 (SV_VL128);
  return svptest_any (svptrue_b64 (), res);
}

void
b16_b8_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b16 (SV_VL32);
  *any = svptest_any (svptrue_b8 (), res);
  *ptr = res;
}

int
b16_b8_2 ()
{
  svbool_t res = svptrue_pat_b16 (SV_VL32);
  return svptest_any (svptrue_b8 (), res);
}

void
b16_b32_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b16 (SV_VL16);
  *any = svptest_any (svptrue_b32 (), res);
  *ptr = res;
}

int
b16_b32_2 ()
{
  svbool_t res = svptrue_pat_b16 (SV_VL16);
  return svptest_any (svptrue_b32 (), res);
}

void
b16_b64_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b16 (SV_VL64);
  *any = svptest_any (svptrue_b64 (), res);
  *ptr = res;
}

int
b16_b64_2 ()
{
  svbool_t res = svptrue_pat_b16 (SV_VL64);
  return svptest_any (svptrue_b64 (), res);
}

void
b32_b8_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b32 (SV_VL16);
  *any = svptest_any (svptrue_b8 (), res);
  *ptr = res;
}

int
b32_b8_2 ()
{
  svbool_t res = svptrue_pat_b32 (SV_VL16);
  return svptest_any (svptrue_b8 (), res);
}

void
b32_b16_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b32 (SV_VL6);
  *any = svptest_any (svptrue_b16 (), res);
  *ptr = res;
}

int
b32_b16_2 ()
{
  svbool_t res = svptrue_pat_b32 (SV_VL6);
  return svptest_any (svptrue_b16 (), res);
}

void
b32_b64_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b32 (SV_VL32);
  *any = svptest_any (svptrue_b64 (), res);
  *ptr = res;
}

int
b32_b64_2 ()
{
  svbool_t res = svptrue_pat_b32 (SV_VL32);
  return svptest_any (svptrue_b64 (), res);
}

void
b64_b8_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b64 (SV_VL7);
  *any = svptest_any (svptrue_b8 (), res);
  *ptr = res;
}

int
b64_b8_2 ()
{
  svbool_t res = svptrue_pat_b64 (SV_VL7);
  return svptest_any (svptrue_b8 (), res);
}

void
b64_b16_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b64 (SV_VL16);
  *any = svptest_any (svptrue_b16 (), res);
  *ptr = res;
}

int
b64_b16_2 ()
{
  svbool_t res = svptrue_pat_b64 (SV_VL16);
  return svptest_any (svptrue_b16 (), res);
}

void
b64_b32_1 (int *any, svbool_t *ptr)
{
  svbool_t res = svptrue_pat_b64 (SV_VL32);
  *any = svptest_any (svptrue_b32 (), res);
  *ptr = res;
}

int
b64_b32_2 ()
{
  svbool_t res = svptrue_pat_b64 (SV_VL32);
  return svptest_any (svptrue_b32 (), res);
}

/* { dg-final { scan-assembler-not {\tptrues\n} } } */
/* { dg-final { scan-assembler-times {\tptrue\t} 48 } } */
/* { dg-final { scan-assembler-times {\tptest\t} 24 } } */
