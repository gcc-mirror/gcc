/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */
/* PR target/116075 */

#include <arm_sve.h>

svint8_t f(int8_t t)
{
  svint8_t tt;
  tt = svdup_s8 (0);
  tt = svinsr (tt, t);
  return tt;
}

svint8_t f1(int8_t t)
{
  svint8_t tt;
  tt = svdup_s8 (t);
  tt = svinsr (tt, 0);
  return tt;
}

/* The above 2 functions should not have removed the VEC_SHL_INSERT. */

/* { dg-final { scan-tree-dump-times ".VEC_SHL_INSERT " 2 "optimized" } } */

