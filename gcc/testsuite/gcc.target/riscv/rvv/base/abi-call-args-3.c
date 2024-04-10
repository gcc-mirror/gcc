/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O1" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/* Test args order.  */

/*
** foo1:
**   ...
**   vsm\.v\tv8,0\(a0\)
**   ...
*/
void
foo1 (vbool1_t a, vbool2_t b, vbool4_t c, vbool2_t *out_b)
{
  *out_b = b;
}

/*
** foo2:
**   ...
**   vsm\.v\tv13,0\(a0\)
**   ...
*/
void
foo2 (vbool1_t a, vbool2_t b, vbool4_t c, vbool8_t d, vbool16_t e, vbool32_t f,
      vbool64_t g, vbool64_t *out_g)
{
  *out_g = g;
}

/*
** foo3:
**   ...
**   vsm\.v\tv12,0\(a0\)
**   ...
*/
void
foo3 (vbool1_t a, vint8m4_t b, vbool2_t c, vbool2_t *out_c)
{
  *out_c = c;
}

/*
** foo4:
**   vs8r\.v\tv16,0\(a0\)
**   ...
*/
void
foo4 (vbool1_t a, vint8m4_t b, vbool2_t c, vint8m8_t d, vint8m8_t *out_d)
{
  *out_d = d;
}

/*
** foo5:
**   vl4re8\.v\tv[0-9]+,0\(a0\)
**   ...
**   vs4r\.v\tv[0-9]+,0\(a1\)
**   ...
*/
void
foo5 (vbool1_t a, vint8m8_t b, vint8m8_t c, vint8m4_t d, vint8m4_t *out_d)
{
  *out_d = d;
}

/*
** foo6:
**   vs1r\.v\tv8,0\(a0\)
**   vs8r\.v\tv16,0\(a1\)
**   vs4r\.v\tv12,0\(a2\)
**   vs2r\.v\tv10,0\(a3\)
**   vs1r\.v\tv9,0\(a4\)
**   ...
*/
void
foo6 (vint8m1_t a, vint8m8_t b, vint8m4_t c, vint8m2_t d, vint8m1_t e,
      vint8m1_t *out_a, vint8m8_t *out_b, vint8m4_t *out_c, vint8m2_t *out_d,
      vint8m1_t *out_e)
{
  *out_a = a;
  *out_b = b;
  *out_c = c;
  *out_d = d;
  *out_e = e;
}

/*
** foo7:
**   vl1re8\.v\tv\d+,0\(a0\)
**   vs1r\.v\tv\d+,0\(a1\)
**   ...
*/
void
foo7 (vint8m1_t a1, vint8m1_t a2, vint8m1_t a3, vint8m1_t a4, vint8m1_t a5,
      vint8m1_t a6, vint8m1_t a7, vint8m1_t a8, vint8m1_t a9, vint8m1_t a10,
      vint8m1_t a11, vint8m1_t a12, vint8m1_t a13, vint8m1_t a14, vint8m1_t a15,
      vint8m1_t a16, vint8m1_t a17, vint8m1_t *out_a17)
{
  *out_a17 = a17;
}

/*
** foo8:
**   vl8re8\.v\tv\d+,0\(a0\)
**   vs8r\.v\tv\d+,0\(a1\)
**   ...
*/
void
foo8 (vint8m8_t a1, vint8m8_t a2, vint8m8_t a3, vint8m8_t *out_a3)
{
  *out_a3 = a3;
}
