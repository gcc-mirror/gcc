/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

double __attribute__ ((noinline))
sumation (double sum0, double sum1, double sum2, double sum3, double sum4,
	  double sum5, double sum6, double sum7)
{
  return sum0 + sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + sum7;
}

double
foo (char const *buf, size_t len)
{
  double sum = 0;
  size_t vl = __riscv_vsetvlmax_e8m8 ();
  size_t step = vl * 4;
  const char *it = buf, *end = buf + len;
  for (; it + step <= end;)
    {
      vfloat32m2_t v0 = __riscv_vle32_v_f32m2 ((void *) it, vl);
      it += vl;
      vfloat32m2_t v1 = __riscv_vle32_v_f32m2 ((void *) it, vl);
      it += vl;
      vfloat32m2_t v2 = __riscv_vle32_v_f32m2 ((void *) it, vl);
      it += vl;
      vfloat32m2_t v3 = __riscv_vle32_v_f32m2 ((void *) it, vl);
      it += vl;
      vfloat32m2_t v4 = __riscv_vle32_v_f32m2 ((void *) it, vl);
      it += vl;
      vfloat32m2_t v5 = __riscv_vle32_v_f32m2 ((void *) it, vl);
      it += vl;
      vfloat32m2_t v6 = __riscv_vle32_v_f32m2 ((void *) it, vl);
      it += vl;
      vfloat32m2_t v7 = __riscv_vle32_v_f32m2 ((void *) it, vl);
      it += vl;

      asm volatile("nop" ::: "memory");
      vfloat64m4_t vw0 = __riscv_vfwcvt_f_f_v_f64m4 (v0, vl);
      vfloat64m4_t vw1 = __riscv_vfwcvt_f_f_v_f64m4 (v1, vl);
      vfloat64m4_t vw2 = __riscv_vfwcvt_f_f_v_f64m4 (v2, vl);
      vfloat64m4_t vw3 = __riscv_vfwcvt_f_f_v_f64m4 (v3, vl);
      vfloat64m4_t vw4 = __riscv_vfwcvt_f_f_v_f64m4 (v4, vl);
      vfloat64m4_t vw5 = __riscv_vfwcvt_f_f_v_f64m4 (v5, vl);
      vfloat64m4_t vw6 = __riscv_vfwcvt_f_f_v_f64m4 (v6, vl);
      vfloat64m4_t vw7 = __riscv_vfwcvt_f_f_v_f64m4 (v7, vl);

      asm volatile("nop" ::: "memory");
      double sum0 = __riscv_vfmv_f_s_f64m4_f64 (vw0);
      double sum1 = __riscv_vfmv_f_s_f64m4_f64 (vw1);
      double sum2 = __riscv_vfmv_f_s_f64m4_f64 (vw2);
      double sum3 = __riscv_vfmv_f_s_f64m4_f64 (vw3);
      double sum4 = __riscv_vfmv_f_s_f64m4_f64 (vw4);
      double sum5 = __riscv_vfmv_f_s_f64m4_f64 (vw5);
      double sum6 = __riscv_vfmv_f_s_f64m4_f64 (vw6);
      double sum7 = __riscv_vfmv_f_s_f64m4_f64 (vw7);

      sum += sumation (sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7);
    }
  return sum;
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} { xfail riscv*-*-* } } } */
