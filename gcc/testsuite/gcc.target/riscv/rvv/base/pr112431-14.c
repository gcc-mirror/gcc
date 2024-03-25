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
      
      asm volatile("nop" ::: "memory");
      vint64m2_t vw0 = __riscv_vfwcvt_rtz_x_f_v_i64m2 (v0, vl);
      vint64m2_t vw1 = __riscv_vfwcvt_rtz_x_f_v_i64m2 (v1, vl);
      vint64m2_t vw2 = __riscv_vfwcvt_rtz_x_f_v_i64m2 (v2, vl);
      vint64m2_t vw3 = __riscv_vfwcvt_rtz_x_f_v_i64m2 (v3, vl);
      vint64m2_t vw4 = __riscv_vfwcvt_rtz_x_f_v_i64m2 (v4, vl);
      vint64m2_t vw5 = __riscv_vfwcvt_rtz_x_f_v_i64m2 (v5, vl);
      vint64m2_t vw6 = __riscv_vfwcvt_rtz_x_f_v_i64m2 (v6, vl);
      vint64m2_t vw7 = __riscv_vfwcvt_rtz_x_f_v_i64m2 (v7, vl);

      asm volatile("nop" ::: "memory");
      double sum0 = __riscv_vmv_x_s_i64m2_i64 (vw0);
      double sum1 = __riscv_vmv_x_s_i64m2_i64 (vw1);
      double sum2 = __riscv_vmv_x_s_i64m2_i64 (vw2);
      double sum3 = __riscv_vmv_x_s_i64m2_i64 (vw3);
      double sum4 = __riscv_vmv_x_s_i64m2_i64 (vw4);
      double sum5 = __riscv_vmv_x_s_i64m2_i64 (vw5);
      double sum6 = __riscv_vmv_x_s_i64m2_i64 (vw6);
      double sum7 = __riscv_vmv_x_s_i64m2_i64 (vw7);

      sum += sumation (sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7);
    }
  return sum;
}

double
foo2 (char const *buf, size_t len)
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
      
      asm volatile("nop" ::: "memory");
      vint64m2_t vw0 = __riscv_vfwcvt_x_f_v_i64m2 (v0, vl);
      vint64m2_t vw1 = __riscv_vfwcvt_x_f_v_i64m2 (v1, vl);
      vint64m2_t vw2 = __riscv_vfwcvt_x_f_v_i64m2 (v2, vl);
      vint64m2_t vw3 = __riscv_vfwcvt_x_f_v_i64m2 (v3, vl);
      vint64m2_t vw4 = __riscv_vfwcvt_x_f_v_i64m2 (v4, vl);
      vint64m2_t vw5 = __riscv_vfwcvt_x_f_v_i64m2 (v5, vl);
      vint64m2_t vw6 = __riscv_vfwcvt_x_f_v_i64m2 (v6, vl);
      vint64m2_t vw7 = __riscv_vfwcvt_x_f_v_i64m2 (v7, vl);

      asm volatile("nop" ::: "memory");
      double sum0 = __riscv_vmv_x_s_i64m2_i64 (vw0);
      double sum1 = __riscv_vmv_x_s_i64m2_i64 (vw1);
      double sum2 = __riscv_vmv_x_s_i64m2_i64 (vw2);
      double sum3 = __riscv_vmv_x_s_i64m2_i64 (vw3);
      double sum4 = __riscv_vmv_x_s_i64m2_i64 (vw4);
      double sum5 = __riscv_vmv_x_s_i64m2_i64 (vw5);
      double sum6 = __riscv_vmv_x_s_i64m2_i64 (vw6);
      double sum7 = __riscv_vmv_x_s_i64m2_i64 (vw7);

      sum += sumation (sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7);
    }
  return sum;
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
