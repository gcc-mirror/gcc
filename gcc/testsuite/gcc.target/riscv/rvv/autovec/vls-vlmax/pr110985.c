/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -O3 -mrvv-vector-bits=zvl -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint-gcc.h>

typedef int16_t vnx16i __attribute__ ((vector_size (32)));

/*
** foo1:
**   vsetivli\s+zero,\s*16,\s*e16,\s*m1,\s*t[au],\s*m[au]
**   vid\.v\s+v[0-9]+
**   vrsub\.vi\s+v[0-9]+,\s*v[0-9]+,\s*15
**   vs1r\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**   ret
*/
void
foo1 (int16_t *__restrict out)
{
  vnx16i v = {15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
  *(vnx16i *) out = v;
}

/*
** foo2:
**   vsetivli\s+zero,\s*16,\s*e16,\s*m1,\s*t[au],\s*m[au]
**   vid\.v\s+v[0-9]+
**   li\s+[a-x0-9]+,\s*7
**   vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**   vadd\.vi\s+v[0-9]+,\s*v[0-9]+,\s*3
**   vs1r\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**   ret
*/
void
foo2 (int16_t *__restrict out)
{
  vnx16i v
    = {3,	   3 + 7 * 1,  3 + 7 * 2,  3 + 7 * 3, 3 + 7 * 4,  3 + 7 * 5,
       3 + 7 * 6,  3 + 7 * 7,  3 + 7 * 8,  3 + 7 * 9, 3 + 7 * 10, 3 + 7 * 11,
       3 + 7 * 12, 3 + 7 * 13, 3 + 7 * 14, 3 + 7 * 15};
  *(vnx16i *) out = v;
}

/*
** foo3:
**   vsetivli\s+zero,\s*16,\s*e16,\s*m1,\s*t[au],\s*m[au]
**   vid\.v\s+v[0-9]+
**   vs1r\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**   ret
*/
void
foo3 (int16_t *__restrict out)
{
  vnx16i v
    = {0, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  *(vnx16i *) out = v;
}

/*
** foo4:
**   vsetivli\s+zero,\s*16,\s*e16,\s*m1,\s*t[au],\s*m[au]
**   vid\.v\s+v[0-9]+
**   li\s+[a-x0-9]+,\s*6
**   vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
**   vs1r\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**   ret
*/
void
foo4 (int16_t *__restrict out)
{
  vnx16i v
    = {0*6, 1*6,2*6,3*6,4*6,5*6,6*6,7*6,8*6,9*6,10*6,11*6,12*6,13*6,14*6,15*6};
  *(vnx16i *) out = v;
}

/*
** foo5:
**   vsetivli\s+zero,\s*16,\s*e16,\s*m1,\s*t[au],\s*m[au]
**   vid\.v\s+v[0-9]+
**   vadd\.vi\s+v[0-9]+,\s*v[0-9]+,\s*-16
**   vs1r\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**   ret
*/
void
foo5 (int16_t *__restrict out)
{
  vnx16i v
    = {0-16, 1-16,2-16,3-16,4-16,5-16,6-16,7-16,8-16,9-16,10-16,11-16,12-16,13-16,14-16,15-16};
  *(vnx16i *) out = v;
}
