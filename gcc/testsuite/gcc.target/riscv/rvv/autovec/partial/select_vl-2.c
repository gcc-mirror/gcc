/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=scalable -fno-schedule-insns -mrvv-max-lmul=m1 -O3 -ftree-vectorize" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint-gcc.h>

/*
** foo:
**	vsetivli\t[a-x0-9]+,\s*8,\s*e(8?|16?|32?|64),\s*m(1?|2?|4?|8?|f2?|f4?|f8),\s*t[au],\s*m[au]
**	...
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	...
**	vsetvli\tzero,\s*[a-x0-9]+,\s*e(8?|16?|32?|64),\s*m(1?|2?|4?|8?|f2?|f4?|f8),\s*t[au],\s*m[au]
**	...
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	...
*/
void
foo (int32_t *__restrict a,
     int32_t *__restrict b,
     int32_t *__restrict cond)
{
  for (int i = 0; i < 8; i++)
    if (cond[i])
      a[i] = b[i];
}
