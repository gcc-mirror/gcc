/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcv -mabi=lp64d" } */

#include <riscv_vector.h>

typedef int (*fn_t) (int);

int __attribute__ ((noinline))
victim (float *p, int n, fn_t fn)
{
  register fn_t target asm ("t1") = fn;

  size_t vl = __riscv_vsetvl_e32m4 (n);
  volatile vfloat32m4_t v0 = __riscv_vle32_v_f32m4 (p, vl);
  volatile vfloat32m4_t v1 = __riscv_vle32_v_f32m4 (p + vl, vl);
  vfloat32m4_t x0 = v0;
  vfloat32m4_t x1 = v1;

  x0 = __riscv_vfadd_vv_f32m4 (x0, x1, vl);
  __riscv_vse32_v_f32m4 (p, x0, vl);
  asm volatile ("" : "+r" (target));

  return target (n);
}

/* { dg-final { scan-assembler {csrr\t[^,]+,vlenb} } } */
/* { dg-final { scan-assembler {\tjr\t(t[2-6]|a[0-7])\n} } } */
/* { dg-final { scan-assembler-not {\tjr\tt[01]\n} } } */
