/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O3" } */

#include "riscv_vector.h"

void
test ()
{
  __riscv_vand ();              /* { dg-error {no matching function call to '__riscv_vand' with empty arguments} } */
  __riscv_vand_tu ();           /* { dg-error {no matching function call to '__riscv_vand_tu' with empty arguments} } */
  __riscv_vand_tumu ();         /* { dg-error {no matching function call to '__riscv_vand_tumu' with empty arguments} } */

  __riscv_vcompress ();         /* { dg-error {no matching function call to '__riscv_vcompress' with empty arguments} } */
  __riscv_vcompress_tu ();      /* { dg-error {no matching function call to '__riscv_vcompress_tu' with empty arguments} } */

  __riscv_vcpop ();             /* { dg-error {no matching function call to '__riscv_vcpop' with empty arguments} } */

  __riscv_vdiv ();              /* { dg-error {no matching function call to '__riscv_vdiv' with empty arguments} } */
  __riscv_vdiv_tu ();           /* { dg-error {no matching function call to '__riscv_vdiv_tu' with empty arguments} } */
  __riscv_vdiv_tumu ();         /* { dg-error {no matching function call to '__riscv_vdiv_tumu' with empty arguments} } */

  __riscv_vfabs ();             /* { dg-error {no matching function call to '__riscv_vfabs' with empty arguments} } */
  __riscv_vfabs_tu ();          /* { dg-error {no matching function call to '__riscv_vfabs_tu' with empty arguments} } */
  __riscv_vfabs_tumu ();        /* { dg-error {no matching function call to '__riscv_vfabs_tumu' with empty arguments} } */

  __riscv_vfadd ();             /* { dg-error {no matching function call to '__riscv_vfadd' with empty arguments} } */
  __riscv_vfadd_tu ();          /* { dg-error {no matching function call to '__riscv_vfadd_tu' with empty arguments} } */
  __riscv_vfadd_tumu ();        /* { dg-error {no matching function call to '__riscv_vfadd_tumu' with empty arguments} } */

  __riscv_vfclass ();           /* { dg-error {no matching function call to '__riscv_vfclass' with empty arguments} } */
  __riscv_vfclass_tu ();        /* { dg-error {no matching function call to '__riscv_vfclass_tu' with empty arguments} } */
  __riscv_vfclass_tumu ();      /* { dg-error {no matching function call to '__riscv_vfclass_tumu' with empty arguments} } */

  __riscv_vfcvt_x ();           /* { dg-error {no matching function call to '__riscv_vfcvt_x' with empty arguments} } */
  __riscv_vfcvt_x_tu ();        /* { dg-error {no matching function call to '__riscv_vfcvt_x_tu' with empty arguments} } */
  __riscv_vfcvt_x_tumu ();      /* { dg-error {no matching function call to '__riscv_vfcvt_x_tumu' with empty arguments} } */

  __riscv_vfirst ();            /* { dg-error {no matching function call to '__riscv_vfirst' with empty arguments} } */

  __riscv_vfmadd ();            /* { dg-error {no matching function call to '__riscv_vfmadd' with empty arguments} } */
  __riscv_vfmadd_tu ();         /* { dg-error {no matching function call to '__riscv_vfmadd_tu' with empty arguments} } */
  __riscv_vfmadd_tumu ();       /* { dg-error {no matching function call to '__riscv_vfmadd_tumu' with empty arguments} } */

  __riscv_vfmerge ();           /* { dg-error {no matching function call to '__riscv_vfmerge' with empty arguments} } */
  __riscv_vfmerge_tu ();        /* { dg-error {no matching function call to '__riscv_vfmerge_tu' with empty arguments} } */

  __riscv_vfncvt_x ();          /* { dg-error {no matching function call to '__riscv_vfncvt_x' with empty arguments} } */
  __riscv_vfncvt_x_tu ();       /* { dg-error {no matching function call to '__riscv_vfncvt_x_tu' with empty arguments} } */
  __riscv_vfncvt_x_tumu ();     /* { dg-error {no matching function call to '__riscv_vfncvt_x_tumu' with empty arguments} } */

  __riscv_vfrec7 ();            /* { dg-error {no matching function call to '__riscv_vfrec7' with empty arguments} } */
  __riscv_vfrec7_tu ();         /* { dg-error {no matching function call to '__riscv_vfrec7_tu' with empty arguments} } */
  __riscv_vfrec7_tumu ();       /* { dg-error {no matching function call to '__riscv_vfrec7_tumu' with empty arguments} } */

  __riscv_vfrsqrt7 ();          /* { dg-error {no matching function call to '__riscv_vfrsqrt7' with empty arguments} } */
  __riscv_vfrsqrt7_tu ();       /* { dg-error {no matching function call to '__riscv_vfrsqrt7_tu' with empty arguments} } */
  __riscv_vfrsqrt7_tumu ();     /* { dg-error {no matching function call to '__riscv_vfrsqrt7_tumu' with empty arguments} } */

  __riscv_vfsgnjn ();           /* { dg-error {no matching function call to '__riscv_vfsgnjn' with empty arguments} } */
  __riscv_vfsgnjn_tu ();        /* { dg-error {no matching function call to '__riscv_vfsgnjn_tu' with empty arguments} } */
  __riscv_vfsgnjn_tumu ();     /* { dg-error {no matching function call to '__riscv_vfsgnjn_tumu' with empty arguments} } */

  __riscv_vfslide1down ();      /* { dg-error {no matching function call to '__riscv_vfslide1down' with empty arguments} } */
  __riscv_vfslide1down_tu ();   /* { dg-error {no matching function call to '__riscv_vfslide1down_tu' with empty arguments} } */
  __riscv_vfslide1down_tumu (); /* { dg-error {no matching function call to '__riscv_vfslide1down_tumu' with empty arguments} } */

  __riscv_vfwmul ();            /* { dg-error {no matching function call to '__riscv_vfwmul' with empty arguments} } */
  __riscv_vfwmul_tu ();         /* { dg-error {no matching function call to '__riscv_vfwmul_tu' with empty arguments} } */
  __riscv_vfwmul_tumu ();       /* { dg-error {no matching function call to '__riscv_vfwmul_tumu' with empty arguments} } */

  __riscv_vle32 ();             /* { dg-error {no matching function call to '__riscv_vle32' with empty arguments} } */
  __riscv_vle32_tu ();          /* { dg-error {no matching function call to '__riscv_vle32_tu' with empty arguments} } */
  __riscv_vle32_tumu ();        /* { dg-error {no matching function call to '__riscv_vle32_tumu' with empty arguments} } */

  __riscv_vlse64 ();            /* { dg-error {no matching function call to '__riscv_vlse64' with empty arguments} } */
  __riscv_vlse64_tu ();         /* { dg-error {no matching function call to '__riscv_vlse64_tu' with empty arguments} } */
  __riscv_vlse64_tumu ();       /* { dg-error {no matching function call to '__riscv_vlse64_tumu' with empty arguments} } */

  __riscv_vmfeq ();             /* { dg-error {no matching function call to '__riscv_vmfeq' with empty arguments} } */

  __riscv_vreinterpret_u8m1 (); /* { dg-error {no matching function call to '__riscv_vreinterpret_u8m1' with empty arguments} } */

  __riscv_vfredosum ();         /* { dg-error {no matching function call to '__riscv_vfredosum' with empty arguments} } */
  __riscv_vfredosum_tu ();      /* { dg-error {no matching function call to '__riscv_vfredosum_tu' with empty arguments} } */

  __riscv_vaadd ();             /* { dg-error {no matching function call to '__riscv_vaadd' with empty arguments} } */

  __riscv_vaaddu ();            /* { dg-error {no matching function call to '__riscv_vaaddu' with empty arguments} } */

  __riscv_vadc ();              /* { dg-error {no matching function call to '__riscv_vadc' with empty arguments} } */

  __riscv_vnmsac ();            /* { dg-error {no matching function call to '__riscv_vnmsac' with empty arguments} } */

  __riscv_vnsrl ();             /* { dg-error {no matching function call to '__riscv_vnsrl' with empty arguments} } */

  __riscv_vfnmadd ();           /* { dg-error {no matching function call to '__riscv_vfnmadd' with empty arguments} } */

  __riscv_vfwsub_vv ();         /* { dg-error {no matching function call to '__riscv_vfwsub_vv' with empty arguments} } */

  __riscv_vfwredosum ();        /* { dg-error {no matching function call to '__riscv_vfwredosum' with empty arguments} } */
}
