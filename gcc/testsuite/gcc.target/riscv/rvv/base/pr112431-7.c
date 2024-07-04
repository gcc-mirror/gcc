/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

double __attribute__ ((noinline))
sumation (double sum0, double sum1, double sum2, double sum3, double sum4,
	  double sum5, double sum6, double sum7, double sum8, double sum9,
	  double sum10, double sum11, double sum12, double sum13, double sum14,
	  double sum15)
{
  return sum0 + sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + sum7 + sum8 + sum9
	 + sum10 + sum11 + sum12 + sum13 + sum14 + sum15;
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
      vfloat32m1_t v0 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v1 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v2 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v3 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v4 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v5 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v6 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v7 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v8 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v9 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v10 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v11 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v12 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v13 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v14 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      vfloat32m1_t v15 = __riscv_vle32_v_f32m1 ((void *) it, vl);
      it += vl;
      
      asm volatile("nop" ::: "memory");
      vfloat64m2_t vw0 = __riscv_vfwcvt_f_f_v_f64m2 (v0, vl);
      vfloat64m2_t vw1 = __riscv_vfwcvt_f_f_v_f64m2 (v1, vl);
      vfloat64m2_t vw2 = __riscv_vfwcvt_f_f_v_f64m2 (v2, vl);
      vfloat64m2_t vw3 = __riscv_vfwcvt_f_f_v_f64m2 (v3, vl);
      vfloat64m2_t vw4 = __riscv_vfwcvt_f_f_v_f64m2 (v4, vl);
      vfloat64m2_t vw5 = __riscv_vfwcvt_f_f_v_f64m2 (v5, vl);
      vfloat64m2_t vw6 = __riscv_vfwcvt_f_f_v_f64m2 (v6, vl);
      vfloat64m2_t vw7 = __riscv_vfwcvt_f_f_v_f64m2 (v7, vl);
      vfloat64m2_t vw8 = __riscv_vfwcvt_f_f_v_f64m2 (v8, vl);
      vfloat64m2_t vw9 = __riscv_vfwcvt_f_f_v_f64m2 (v9, vl);
      vfloat64m2_t vw10 = __riscv_vfwcvt_f_f_v_f64m2 (v10, vl);
      vfloat64m2_t vw11 = __riscv_vfwcvt_f_f_v_f64m2 (v11, vl);
      vfloat64m2_t vw12 = __riscv_vfwcvt_f_f_v_f64m2 (v12, vl);
      vfloat64m2_t vw13 = __riscv_vfwcvt_f_f_v_f64m2 (v13, vl);
      vfloat64m2_t vw14 = __riscv_vfwcvt_f_f_v_f64m2 (v14, vl);
      vfloat64m2_t vw15 = __riscv_vfwcvt_f_f_v_f64m2 (v15, vl);

      asm volatile("nop" ::: "memory");
      double sum0 = __riscv_vfmv_f_s_f64m2_f64 (vw0);
      double sum1 = __riscv_vfmv_f_s_f64m2_f64 (vw1);
      double sum2 = __riscv_vfmv_f_s_f64m2_f64 (vw2);
      double sum3 = __riscv_vfmv_f_s_f64m2_f64 (vw3);
      double sum4 = __riscv_vfmv_f_s_f64m2_f64 (vw4);
      double sum5 = __riscv_vfmv_f_s_f64m2_f64 (vw5);
      double sum6 = __riscv_vfmv_f_s_f64m2_f64 (vw6);
      double sum7 = __riscv_vfmv_f_s_f64m2_f64 (vw7);
      double sum8 = __riscv_vfmv_f_s_f64m2_f64 (vw8);
      double sum9 = __riscv_vfmv_f_s_f64m2_f64 (vw9);
      double sum10 = __riscv_vfmv_f_s_f64m2_f64 (vw10);
      double sum11 = __riscv_vfmv_f_s_f64m2_f64 (vw11);
      double sum12 = __riscv_vfmv_f_s_f64m2_f64 (vw12);
      double sum13 = __riscv_vfmv_f_s_f64m2_f64 (vw13);
      double sum14 = __riscv_vfmv_f_s_f64m2_f64 (vw14);
      double sum15 = __riscv_vfmv_f_s_f64m2_f64 (vw15);

      sum += sumation (sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7, sum8,
		       sum9, sum10, sum11, sum12, sum13, sum14, sum15);
    }
  return sum;
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} { xfail riscv*-*-* } } } */
