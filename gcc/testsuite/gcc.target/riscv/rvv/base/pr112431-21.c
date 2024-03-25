/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-O3 -ansi -pedantic-errors -std=gnu99" } */

#include <riscv_vector.h>

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
foo (short const *buf, size_t len)
{
  size_t sum = 0;
  size_t vl = 4;
  const short *it = buf;
  for (int i = 0; i < len; i++)
    {
      vint16m2_t v0 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v1 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v2 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v3 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v4 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v5 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v6 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v7 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v8 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v9 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v10 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v11 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v12 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v13 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v14 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;
      vint16m2_t v15 = __riscv_vle16_v_i16m2 (it, vl);
      it += vl;

      asm volatile ("" ::: "memory");
      vint16m2_t vw0 = __riscv_vwadd_wx_i16m2 (v0, 55, vl);
      vint16m2_t vw1 = __riscv_vwadd_wx_i16m2 (v1, 55, vl);
      vint16m2_t vw2 = __riscv_vwadd_wx_i16m2 (v2, 55, vl);
      vint16m2_t vw3 = __riscv_vwadd_wx_i16m2 (v3, 55, vl);
      vint16m2_t vw4 = __riscv_vwadd_wx_i16m2 (v4, 55, vl);
      vint16m2_t vw5 = __riscv_vwadd_wx_i16m2 (v5, 55, vl);
      vint16m2_t vw6 = __riscv_vwadd_wx_i16m2 (v6, 55, vl);
      vint16m2_t vw7 = __riscv_vwadd_wx_i16m2 (v7, 55, vl);
      vint16m2_t vw8 = __riscv_vwadd_wx_i16m2 (v8, 55, vl);
      vint16m2_t vw9 = __riscv_vwadd_wx_i16m2 (v9, 55, vl);
      vint16m2_t vw10 = __riscv_vwadd_wx_i16m2 (v10, 55, vl);
      vint16m2_t vw11 = __riscv_vwadd_wx_i16m2 (v11, 55, vl);
      vint16m2_t vw12 = __riscv_vwadd_wx_i16m2 (v12, 55, vl);
      vint16m2_t vw13 = __riscv_vwadd_wx_i16m2 (v13, 55, vl);
      vint16m2_t vw14 = __riscv_vwadd_wx_i16m2 (v14, 55, vl);
      vint16m2_t vw15 = __riscv_vwadd_wx_i16m2 (v15, 55, vl);

      asm volatile ("" ::: "memory");
      size_t sum0 = __riscv_vmv_x_s_i16m2_i16 (vw0);
      size_t sum1 = __riscv_vmv_x_s_i16m2_i16 (vw1);
      size_t sum2 = __riscv_vmv_x_s_i16m2_i16 (vw2);
      size_t sum3 = __riscv_vmv_x_s_i16m2_i16 (vw3);
      size_t sum4 = __riscv_vmv_x_s_i16m2_i16 (vw4);
      size_t sum5 = __riscv_vmv_x_s_i16m2_i16 (vw5);
      size_t sum6 = __riscv_vmv_x_s_i16m2_i16 (vw6);
      size_t sum7 = __riscv_vmv_x_s_i16m2_i16 (vw7);
      size_t sum8 = __riscv_vmv_x_s_i16m2_i16 (vw8);
      size_t sum9 = __riscv_vmv_x_s_i16m2_i16 (vw9);
      size_t sum10 = __riscv_vmv_x_s_i16m2_i16 (vw10);
      size_t sum11 = __riscv_vmv_x_s_i16m2_i16 (vw11);
      size_t sum12 = __riscv_vmv_x_s_i16m2_i16 (vw12);
      size_t sum13 = __riscv_vmv_x_s_i16m2_i16 (vw13);
      size_t sum14 = __riscv_vmv_x_s_i16m2_i16 (vw14);
      size_t sum15 = __riscv_vmv_x_s_i16m2_i16 (vw15);

      sum += sumation (sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7, sum8,
                       sum9, sum10, sum11, sum12, sum13, sum14, sum15);
    }
  return sum;
}

int
main (int in, char **out)
{
  short const buf[1000];
  int i = foo (buf, 4);
  **out = i;
  return 0;
}
