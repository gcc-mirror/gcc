/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8" } */

#include "def.h"

DEF_REPEAT (v16di, int64_t, 16, var0, var1, var0, var1, var0, var1, var0, var1,
	    var0, var1, var0, var1, var0, var1, var0, var1)
DEF_REPEAT (v32di, int64_t, 32, var0, var1, var0, var1, var0, var1, var0, var1,
	    var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
	    var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
	    var0, var1)
DEF_REPEAT (v64di, int64_t, 64, var0, var1, var0, var1, var0, var1, var0, var1,
	    var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
	    var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
	    var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
	    var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
	    var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
	    var1)
DEF_REPEAT (v128di, int64_t, 128, var0, var1, var0, var1, var0, var1, var0,
	    var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
	    var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
	    var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
	    var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
	    var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
	    var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
	    var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
	    var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
	    var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
	    var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
	    var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1)
DEF_REPEAT (
  v256di, int64_t, 256, var0, var1, var0, var1, var0, var1, var0, var1, var0,
  var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
  var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
  var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
  var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
  var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
  var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
  var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
  var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
  var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
  var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
  var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
  var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
  var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
  var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
  var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
  var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
  var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1,
  var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0,
  var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1, var0, var1)

/* { dg-final { scan-assembler-times {vmv\.v\.x\s+v0,\s*[atx][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmv\.s\.x\s+v0,\s*[atx][0-9]+} 3 } } */
/* { dg-final { scan-assembler-not {vmv1r\.v\s+v0,\s*v[0-9]+} } } */
