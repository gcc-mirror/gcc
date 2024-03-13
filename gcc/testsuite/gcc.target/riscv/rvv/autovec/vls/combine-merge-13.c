/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_REPEAT (v16sf, float, 16, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var0, var0, var0, var0, var0, var0)
DEF_REPEAT (v32sf, float, 32, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var0, var0, var0, var0, var0, var0)
DEF_REPEAT (v64sf, float, 64, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var0, var0, var0, var0)
DEF_REPEAT (v128sf, float, 128, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var0, var0, var0, var0, var0, var0)
DEF_REPEAT (
  v256sf, float, 256, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var0, var0, var0, var0, var0, var0)
DEF_REPEAT (
  v512sf, float, 512, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
  var1, var1, var1, var1, var1, var1, var1, var0, var0, var0, var0, var0, var0)


/* { dg-final { scan-assembler-times {vfmv\.v\.f} 6 } } */
/* { dg-final { scan-assembler-times {vid\.v} 6 } } */
/* { dg-final { scan-assembler-times {vmsgtu} 6 } } */
/* { dg-final { scan-assembler-times {vfmerge\.vfm} 6 } } */
