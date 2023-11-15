/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv_zvl4096b -mabi=lp64d -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint-gcc.h>

typedef int64_t vnx512di __attribute__ ((vector_size (512 * 8)));

/*
** f_vnx512di:
**   ...
**   vsetivli\s+zero,\s*8,\s*e64,\s*m1,\s*ta,\s*ma
**   vmv\.v\.x\s+v0,[atx][0-9]+
**   vsetvli\s+[atx][0-9]+,\s*zero,\s*e64,\s*m8,\s*ta,\s*ma
**   vmerge\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[axt][0-9]+,\s*v0
**   vs8r\.v\s+v[0-9]+,\s*0\([axt][0-9]+\)
**   ret
*/
__attribute__ ((noipa)) void
f_vnx512di (int64_t a, int64_t b, int64_t *out)
{
  vnx512di v = {
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
    a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b,
  };
  *(vnx512di *) out = v;
}
