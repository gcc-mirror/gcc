/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O1" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

#define FOO_(TYPE)                                                             \
  TYPE foo_##TYPE (TYPE *out)                                                  \
  {                                                                            \
    return *out;                                                               \
  }

/* Test the first vector mask type argument */

/*
** foo_vbool1_t:
**   ...
**   vlm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool1_t)

/*
** foo_vbool2_t:
**   ...
**   vlm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool2_t)

/*
** foo_vbool4_t:
**   ...
**   vlm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool4_t)

/*
** foo_vbool8_t:
**   ...
**   vlm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool8_t)

/*
** foo_vbool16_t:
**   ...
**   vlm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool16_t)

/*
** foo_vbool32_t:
**   ...
**   vlm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool32_t)

/*
** foo_vbool64_t:
**   ...
**   vlm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool64_t)

/* Test the first vector data type argument */

/*
** foo_vint8mf8_t:
**   ...
**   vle8\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8mf8_t)

/*
** foo_vint8mf4_t:
**   ...
**   vle8\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8mf4_t)

/*
** foo_vint8mf2_t:
**   ...
**   vle8\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8mf2_t)

/*
** foo_vint8m1_t:
**   vl1re8\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8m1_t)

/*
** foo_vint8m2_t:
**   vl2re8\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8m2_t)

/*
** foo_vint8m4_t:
**   vl4re8\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8m4_t)

/*
** foo_vint8m8_t:
**   vl8re8\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8m8_t)

/*
** foo_vint8m1x5_t:
**   ...
**   vl1re8\.v\tv8,0\(a0\)
**   ...
**   vl1re8\.v\tv9,0\(a\d+\)
**   ...
**   vl1re8\.v\tv10,0\(a\d+\)
**   ...
**   vl1re8\.v\tv11,0\(a\d+\)
**   ...
**   vl1re8\.v\tv12,0\(a\d+\)
**   ...
*/
FOO_ (vint8m1x5_t)

/*
** foo_vint8m1x8_t:
**   ...
**   vl1re8\.v\tv8,0\(a0\)
**   ...
**   vl1re8\.v\tv9,0\(a\d+\)
**   ...
**   vl1re8\.v\tv10,0\(a\d+\)
**   ...
**   vl1re8\.v\tv11,0\(a\d+\)
**   ...
**   vl1re8\.v\tv12,0\(a\d+\)
**   ...
**   vl1re8\.v\tv13,0\(a\d+\)
**   ...
**   vl1re8\.v\tv14,0\(a\d+\)
**   ...
**   vl1re8\.v\tv15,0\(a\d+\)
**   ...
*/
FOO_ (vint8m1x8_t)

/*
** foo_vint8m2x3_t:
**   ...
**   vl2re8\.v\tv8,0\(a0\)
**   ...
**   vl2re8\.v\tv10,0\(a\d+\)
**   ...
**   vl2re8\.v\tv12,0\(a\d+\)
**   ...
*/
FOO_ (vint8m2x3_t)

/*
** foo_vint8m2x4_t:
**   ...
**   vl2re8\.v\tv8,0\(a0\)
**   ...
**   vl2re8\.v\tv10,0\(a\d+\)
**   ...
**   vl2re8\.v\tv12,0\(a\d+\)
**   ...
**   vl2re8\.v\tv14,0\(a\d+\)
**   ...
*/
FOO_ (vint8m2x4_t)

/*
** foo_vint8m4x2_t:
**   ...
**   vl4re8\.v\tv8,0\(a0\)
**   ...
**   vl4re8\.v\tv12,0\(a\d+\)
**   ...
*/
FOO_ (vint8m4x2_t)
