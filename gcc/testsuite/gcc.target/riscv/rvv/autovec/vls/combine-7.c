/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_COMBINE (df, double, 4, x, x, y, y)
DEF_COMBINE (df, double, 8, x, x, x, x, y, y, y, y)
DEF_COMBINE (df, double, 16, x, x, x, x, x, x, x, x, y, y, y, y, y, y, y, y)
DEF_COMBINE (df, double, 32, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, y,
	     y, y, y, y, y, y, y, y, y, y, y, y, y, y, y)
DEF_COMBINE (df, double, 64, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	     x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, y, y, y, y, y, y, y,
	     y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
	     y, y, y)
DEF_COMBINE (df, double, 128, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	     x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	     x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	     x, x, x, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
	     y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
	     y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
	     y)
DEF_COMBINE (df, double, 256, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	     x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	     x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	     x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	     x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	     x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
	     x, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
	     y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
	     y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
	     y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
	     y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
	     y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y)
DEF_COMBINE (
  df, double, 512, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
  x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
  x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
  x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
  x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
  x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
  x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
  x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
  x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
  x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
  x, x, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
  y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
  y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
  y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
  y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
  y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
  y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
  y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
  y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y,
  y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y, y)


/* { dg-final { scan-assembler-times {vslideup\.vx} 4 } } */
/* { dg-final { scan-assembler-times {vslideup\.vi} 4 } } */
