/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zfh -mabi=lp64d -O3" } */

#include "riscv_vector.h"

size_t __attribute__ ((noinline))
sumation (size_t sum0, size_t sum1, size_t sum2, size_t sum3, size_t sum4,
	  size_t sum5, size_t sum6, size_t sum7, size_t sum8, size_t sum9,
	  size_t sum10, size_t sum11, size_t sum12, size_t sum13, size_t sum14,
	  size_t sum15)
{
  return sum0 + sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + sum7 + sum8 + sum9
	 + sum10 + sum11 + sum12 + sum13 + sum14 + sum15;
}

size_t __attribute__ ((noinline))
foo (float const *buf, size_t len)
{
  size_t sum = 0;
  size_t vl = 4;
  const float *it = buf;
  for (int i = 0; i < len; i++)
    {
      vfloat32m2_t v0 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v1 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v2 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v3 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v4 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v5 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v6 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v7 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v8 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v9 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v10 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v11 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v12 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v13 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v14 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;
      vfloat32m2_t v15 = __riscv_vle32_v_f32m2 (it, vl);
      it += vl;

      asm volatile("nop" ::: "memory");
      vfloat32m2_t vw0 = __riscv_vfwadd_wf_f32m2 (v0, 55, vl);
      vfloat32m2_t vw1 = __riscv_vfwadd_wf_f32m2 (v1, 55, vl);
      vfloat32m2_t vw2 = __riscv_vfwadd_wf_f32m2 (v2, 55, vl);
      vfloat32m2_t vw3 = __riscv_vfwadd_wf_f32m2 (v3, 55, vl);
      vfloat32m2_t vw4 = __riscv_vfwadd_wf_f32m2 (v4, 55, vl);
      vfloat32m2_t vw5 = __riscv_vfwadd_wf_f32m2 (v5, 55, vl);
      vfloat32m2_t vw6 = __riscv_vfwadd_wf_f32m2 (v6, 55, vl);
      vfloat32m2_t vw7 = __riscv_vfwadd_wf_f32m2 (v7, 55, vl);
      vfloat32m2_t vw8 = __riscv_vfwadd_wf_f32m2 (v8, 55, vl);
      vfloat32m2_t vw9 = __riscv_vfwadd_wf_f32m2 (v9, 55, vl);
      vfloat32m2_t vw10 = __riscv_vfwadd_wf_f32m2 (v10, 55, vl);
      vfloat32m2_t vw11 = __riscv_vfwadd_wf_f32m2 (v11, 55, vl);
      vfloat32m2_t vw12 = __riscv_vfwadd_wf_f32m2 (v12, 55, vl);
      vfloat32m2_t vw13 = __riscv_vfwadd_wf_f32m2 (v13, 55, vl);
      vfloat32m2_t vw14 = __riscv_vfwadd_wf_f32m2 (v14, 55, vl);
      vfloat32m2_t vw15 = __riscv_vfwadd_wf_f32m2 (v15, 55, vl);

      asm volatile("nop" ::: "memory");
      size_t sum0 = __riscv_vfmv_f_s_f32m2_f32 (vw0);
      size_t sum1 = __riscv_vfmv_f_s_f32m2_f32 (vw1);
      size_t sum2 = __riscv_vfmv_f_s_f32m2_f32 (vw2);
      size_t sum3 = __riscv_vfmv_f_s_f32m2_f32 (vw3);
      size_t sum4 = __riscv_vfmv_f_s_f32m2_f32 (vw4);
      size_t sum5 = __riscv_vfmv_f_s_f32m2_f32 (vw5);
      size_t sum6 = __riscv_vfmv_f_s_f32m2_f32 (vw6);
      size_t sum7 = __riscv_vfmv_f_s_f32m2_f32 (vw7);
      size_t sum8 = __riscv_vfmv_f_s_f32m2_f32 (vw8);
      size_t sum9 = __riscv_vfmv_f_s_f32m2_f32 (vw9);
      size_t sum10 = __riscv_vfmv_f_s_f32m2_f32 (vw10);
      size_t sum11 = __riscv_vfmv_f_s_f32m2_f32 (vw11);
      size_t sum12 = __riscv_vfmv_f_s_f32m2_f32 (vw12);
      size_t sum13 = __riscv_vfmv_f_s_f32m2_f32 (vw13);
      size_t sum14 = __riscv_vfmv_f_s_f32m2_f32 (vw14);
      size_t sum15 = __riscv_vfmv_f_s_f32m2_f32 (vw15);

      sum += sumation (sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7, sum8,
		       sum9, sum10, sum11, sum12, sum13, sum14, sum15);
    }
  return sum;
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
