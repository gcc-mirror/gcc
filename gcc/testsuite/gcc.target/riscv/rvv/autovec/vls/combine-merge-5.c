/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_REPEAT (v16hf, _Float16, 16, var0, var0, var0, var0, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1)
DEF_REPEAT (v32hf, _Float16, 32, var0, var0, var0, var0, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1)
DEF_REPEAT (v64hf, _Float16, 64, var0, var0, var0, var0, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1,
	    var1)
DEF_REPEAT (v128hf, _Float16, 128, var0, var0, var0, var0, var1, var1, var1, var1,
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
	    var1, var1, var1, var1, var1, var1, var1, var1, var1, var1)
DEF_REPEAT (
  v256hf, _Float16, 256, var0, var0, var0, var0, var1, var1, var1, var1, var1,
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
  var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1, var1)
DEF_REPEAT (
  v512hf, _Float16, 512, var0, var0, var0, var0, var1, var1, var1, var1, var1,
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
  var1, var1, var1, var1, var1, var1, var1, var1, var1)


/* { dg-final { scan-assembler-times {vfmv\.v\.f} 6 } } */
/* { dg-final { scan-assembler-times {vid\.v} 6 } } */
/* { dg-final { scan-assembler-times {vmsgtu\.vi} 6 } } */
/* { dg-final { scan-assembler-times {vfmerge\.vfm} 6 } } */
