/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O1" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

#define FOO_(TYPE)                                                             \
  void foo_##TYPE (TYPE val, TYPE *out)                                        \
  {                                                                            \
    *out = val;                                                                \
  }

/* Test the first vector mask type argument */

/*
** foo_vbool1_t:
**   ...
**   vsm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool1_t)

/*
** foo_vbool2_t:
**   ...
**   vsm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool2_t)

/*
** foo_vbool4_t:
**   ...
**   vsm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool4_t)

/*
** foo_vbool8_t:
**   ...
**   vsm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool8_t)

/*
** foo_vbool16_t:
**   ...
**   vsm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool16_t)

/*
** foo_vbool32_t:
**   ...
**   vsm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool32_t)

/*
** foo_vbool64_t:
**   ...
**   vsm\.v\tv0,0\(a0\)
**   ...
*/
FOO_ (vbool64_t)

/* Test the first vector data type argument */

/*
** foo_vint8mf8_t:
**   ...
**   vse8\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8mf8_t)

/*
** foo_vint8mf4_t:
**   ...
**   vse8\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8mf4_t)

/*
** foo_vint8mf2_t:
**   ...
**   vse8\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8mf2_t)

/*
** foo_vint8m1_t:
**   vs1r\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8m1_t)

/*
** foo_vint8m2_t:
**   vs2r\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8m2_t)

/*
** foo_vint8m4_t:
**   vs4r\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8m4_t)

/*
** foo_vint8m8_t:
**   vs8r\.v\tv8,0\(a0\)
**   ...
*/
FOO_ (vint8m8_t)

/*
** foo_vint8m1x5_t:
**   ...
**   vs1r\.v\tv8,0\(a0\)
**   ...
**   vs1r\.v\tv9,0\(a\d+\)
**   ...
**   vs1r\.v\tv10,0\(a\d+\)
**   ...
**   vs1r\.v\tv11,0\(a\d+\)
**   ...
**   vs1r\.v\tv12,0\(a\d+\)
**   ...
*/
FOO_ (vint8m1x5_t)

/*
** foo_vint8m1x8_t:
**   ...
**   vs1r\.v\tv8,0\(a0\)
**   ...
**   vs1r\.v\tv9,0\(a\d+\)
**   ...
**   vs1r\.v\tv10,0\(a\d+\)
**   ...
**   vs1r\.v\tv11,0\(a\d+\)
**   ...
**   vs1r\.v\tv12,0\(a\d+\)
**   ...
**   vs1r\.v\tv13,0\(a\d+\)
**   ...
**   vs1r\.v\tv14,0\(a\d+\)
**   ...
**   vs1r\.v\tv15,0\(a\d+\)
**   ...
*/
FOO_ (vint8m1x8_t)

/*
** foo_vint8m2x3_t:
**   ...
**   vs2r\.v\tv8,0\(a0\)
**   ...
**   vs2r\.v\tv10,0\(a\d+\)
**   ...
**   vs2r\.v\tv12,0\(a\d+\)
**   ...
*/
FOO_ (vint8m2x3_t)

/*
** foo_vint8m2x4_t:
**   ...
**   vs2r\.v\tv8,0\(a0\)
**   ...
**   vs2r\.v\tv10,0\(a\d+\)
**   ...
**   vs2r\.v\tv12,0\(a\d+\)
**   ...
**   vs2r\.v\tv14,0\(a\d+\)
**   ...
*/
FOO_ (vint8m2x4_t)

/*
** foo_vint8m4x2_t:
**   ...
**   vs4r\.v\tv8,0\(a0\)
**   ...
**   vs4r\.v\tv12,0\(a\d+\)
**   ...
*/
FOO_ (vint8m4x2_t)
