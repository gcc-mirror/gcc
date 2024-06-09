/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvl1024b -mabi=lp64d -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint-gcc.h>

typedef int64_t vnx32di __attribute__ ((vector_size (32 * 8)));

/*
** f_vnx32di:
**   vsetvli\s+[axt][0-9]+,\s*zero,\s*e64,\s*m2,\s*ta,\s*ma
**   ...
**   vmv\.v\.x\s+v[0-9]+,\s*[axt][0-9]+
**   ...
**   vmv\.s\.x\s+v0,\s*[axt][0-9]+
**   vmerge\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[axt][0-9]+,\s*v0
**   vs2r\.v\s+v[0-9]+,\s*0\([axt][0-9]+\)
**   ret
*/
__attribute__ ((noipa)) void
f_vnx32di (int64_t a, int64_t b, int64_t *out)
{
  vnx32di v = {
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
  };
  *(vnx32di *) out = v;
}
