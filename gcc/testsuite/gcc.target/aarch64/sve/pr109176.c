/* PR tree-optimization/109176 */
/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

#include <arm_sve.h>

svbool_t
foo (svint8_t a, svint8_t b, svbool_t c)
{
  svbool_t d = svcmplt_s8 (svptrue_pat_b8 (SV_ALL), a, b);
  return svsel_b (d, c, d);
}
