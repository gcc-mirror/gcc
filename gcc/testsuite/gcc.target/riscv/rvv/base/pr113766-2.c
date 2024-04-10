/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O3" } */

#include "riscv_vector.h"

void
test (vint32m1_t vi32m1, vint64m1_t vi64m1, vfloat32m1_t vf32m1, unsigned vl)
{
  __riscv_vand (vi32m1, vl);         /* { dg-error {too few arguments to function '__riscv_vand_vx_i32m1'} } */

  __riscv_vcompress (vi32m1, vl);    /* { dg-error {too many arguments to function '__riscv_vcompress'} } */

  __riscv_vcpop (vi32m1, vl);        /* { dg-error {too many arguments to function '__riscv_vcpop'} } */

  __riscv_vdiv (vi32m1, vl);         /* { dg-error {too few arguments to function '__riscv_vdiv_vx_i32m1'} } */

  __riscv_vfabs (vf32m1);            /* { dg-error {too many arguments to function '__riscv_vfabs'} } */

  __riscv_vfadd (vf32m1, vl);        /* { dg-error {too many arguments to function '__riscv_vfadd'} } */

  __riscv_vfcvt_x (vf32m1);          /* { dg-error {too many arguments to function '__riscv_vfcvt_x'} } */

  __riscv_vfirst (vf32m1, vl);       /* { dg-error {too many arguments to function '__riscv_vfirst'} } */

  __riscv_vfmadd (vf32m1, vl);       /* { dg-error {too many arguments to function '__riscv_vfmadd'} } */

  __riscv_vfmerge (vf32m1, vl);      /* { dg-error {too many arguments to function '__riscv_vfmerge'} } */

  __riscv_vfncvt_x (vf32m1);         /* { dg-error {too many arguments to function '__riscv_vfncvt_x'} } */

  __riscv_vfrec7 (vf32m1);           /* { dg-error {too many arguments to function '__riscv_vfrec7'} } */

  __riscv_vfrsqrt7 (vf32m1);         /* { dg-error {too many arguments to function '__riscv_vfrsqrt7'} } */

  __riscv_vfsgnjn (vf32m1, vl);      /* { dg-error {too few arguments to function '__riscv_vfsgnjn_vf_f32m1'} } */

  __riscv_vfslide1down (vf32m1, vl); /* { dg-error {too few arguments to function '__riscv_vfslide1down_vf_f32m1'} } */

  __riscv_vfwmul (vf32m1, vl);       /* { dg-error {too many arguments to function '__riscv_vfwmul'} } */

  __riscv_vle32 (vi32m1, vl);        /* { dg-error {too many arguments to function '__riscv_vle32'} } */

  __riscv_vlse64 (vi64m1, vl);       /* { dg-error {too many arguments to function '__riscv_vlse64'} } */

  __riscv_vmfeq (vf32m1, vl);        /* { dg-error {too few arguments to function '__riscv_vmfeq_vf_f32m1_b32'} } */

  __riscv_vfredosum (vf32m1, vl);    /* { dg-error {too many arguments to function '__riscv_vfredosum'} } */
}
