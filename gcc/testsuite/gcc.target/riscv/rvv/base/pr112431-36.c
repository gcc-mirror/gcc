/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

size_t __attribute__ ((noinline))
sumation (size_t sum0, size_t sum1, size_t sum2, size_t sum3, size_t sum4,
	  size_t sum5, size_t sum6, size_t sum7,
	  size_t sum0_2, size_t sum1_2, size_t sum2_2, size_t sum3_2, size_t sum4_2,
	  size_t sum5_2, size_t sum6_2, size_t sum7_2)
{
  return sum0 + sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + sum7
  + sum0_2 + sum1_2 + sum2_2 + sum3_2 + sum4_2 + sum5_2 + sum6_2 + sum7_2;
}

size_t
foo (char const *buf, size_t len)
{
  size_t sum = 0;
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

      vfloat64m1_t vw0 = __riscv_vle64_v_f64m1 ((void *) it, vl);
      it += vl;
      vfloat64m1_t vw1 = __riscv_vle64_v_f64m1 ((void *) it, vl);
      it += vl;
      vfloat64m1_t vw2 = __riscv_vle64_v_f64m1 ((void *) it, vl);
      it += vl;
      vfloat64m1_t vw3 = __riscv_vle64_v_f64m1 ((void *) it, vl);
      it += vl;
      vfloat64m1_t vw4 = __riscv_vle64_v_f64m1 ((void *) it, vl);
      it += vl;
      vfloat64m1_t vw5 = __riscv_vle64_v_f64m1 ((void *) it, vl);
      it += vl;
      vfloat64m1_t vw6 = __riscv_vle64_v_f64m1 ((void *) it, vl);
      it += vl;
      vfloat64m1_t vw7 = __riscv_vle64_v_f64m1 ((void *) it, vl);
      it += vl;
      
      asm volatile("nop" ::: "memory");
      vfloat64m1_t vw0_2 = __riscv_vfwredusum_vs_f32m2_f64m1 (v0, vw0, vl);
      vfloat64m1_t vw1_2 = __riscv_vfwredusum_vs_f32m2_f64m1 (v1, vw1, vl);
      vfloat64m1_t vw2_2 = __riscv_vfwredusum_vs_f32m2_f64m1 (v2, vw2, vl);
      vfloat64m1_t vw3_2 = __riscv_vfwredusum_vs_f32m2_f64m1 (v3, vw3, vl);
      vfloat64m1_t vw4_2 = __riscv_vfwredusum_vs_f32m2_f64m1 (v4, vw4, vl);
      vfloat64m1_t vw5_2 = __riscv_vfwredusum_vs_f32m2_f64m1 (v5, vw5, vl);
      vfloat64m1_t vw6_2 = __riscv_vfwredusum_vs_f32m2_f64m1 (v6, vw6, vl);
      vfloat64m1_t vw7_2 = __riscv_vfwredusum_vs_f32m2_f64m1 (v7, vw7, vl);
      
      vw0 = __riscv_vfwredusum_vs_f32m2_f64m1 (v0, vw0_2, vl);
      vw1 = __riscv_vfwredusum_vs_f32m2_f64m1 (v1, vw1_2, vl);
      vw2 = __riscv_vfwredusum_vs_f32m2_f64m1 (v2, vw2_2, vl);
      vw3 = __riscv_vfwredusum_vs_f32m2_f64m1 (v3, vw3_2, vl);
      vw4 = __riscv_vfwredusum_vs_f32m2_f64m1 (v4, vw4_2, vl);
      vw5 = __riscv_vfwredusum_vs_f32m2_f64m1 (v5, vw5_2, vl);
      vw6 = __riscv_vfwredusum_vs_f32m2_f64m1 (v6, vw6_2, vl);
      vw7 = __riscv_vfwredusum_vs_f32m2_f64m1 (v7, vw7_2, vl);

      asm volatile("nop" ::: "memory");
      size_t sum0 = __riscv_vfmv_f_s_f64m1_f64 (vw0);
      size_t sum1 = __riscv_vfmv_f_s_f64m1_f64 (vw1);
      size_t sum2 = __riscv_vfmv_f_s_f64m1_f64 (vw2);
      size_t sum3 = __riscv_vfmv_f_s_f64m1_f64 (vw3);
      size_t sum4 = __riscv_vfmv_f_s_f64m1_f64 (vw4);
      size_t sum5 = __riscv_vfmv_f_s_f64m1_f64 (vw5);
      size_t sum6 = __riscv_vfmv_f_s_f64m1_f64 (vw6);
      size_t sum7 = __riscv_vfmv_f_s_f64m1_f64 (vw7);

      size_t sum0_2 = __riscv_vfmv_f_s_f64m1_f64 (vw0_2);
      size_t sum1_2 = __riscv_vfmv_f_s_f64m1_f64 (vw1_2);
      size_t sum2_2 = __riscv_vfmv_f_s_f64m1_f64 (vw2_2);
      size_t sum3_2 = __riscv_vfmv_f_s_f64m1_f64 (vw3_2);
      size_t sum4_2 = __riscv_vfmv_f_s_f64m1_f64 (vw4_2);
      size_t sum5_2 = __riscv_vfmv_f_s_f64m1_f64 (vw5_2);
      size_t sum6_2 = __riscv_vfmv_f_s_f64m1_f64 (vw6_2);
      size_t sum7_2 = __riscv_vfmv_f_s_f64m1_f64 (vw7_2);

      sum += sumation (sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7,
      sum0_2, sum1_2, sum2_2, sum3_2, sum4_2, sum5_2, sum6_2, sum7_2);
    }
  return sum;
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
