/* { dg-do run { target { rv64 } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-require-effective-target riscv_zvfh_ok } */
/* { dg-options " -march=rv64gcv_zvfh -mabi=lp64d -O2 --param=vsetvl-strategy=optim -fno-schedule-insns  -fno-schedule-insns2 -fno-schedule-fusion " } */

#include "riscv_vector.h"

void
__attribute__ ((noipa))
foo (vfloat16m4_t *v1, vuint32m8_t *v2, vuint8m2_t *v3, size_t vl)
{
  *v1 = __riscv_vfmv_s_f_f16m4 (1, vl);
  *v2 = __riscv_vmv_s_x_u32m8 (2963090659u, vl);
  *v3 = __riscv_vsll_vx_u8m2 (__riscv_vid_v_u8m2 (vl), 2, vl);
}

int
main ()
{
  vfloat16m4_t v1;
  vuint32m8_t v2;
  vuint8m2_t v3;
  int vl = 4;
  foo (&v1, &v2, &v3, vl);

  _Float16 val1 = ((_Float16 *)&v1)[0];
  if (val1 - 1.0000f > 0.00001f)
    __builtin_abort ();

  uint32_t val2 = ((uint32_t *)&v2)[0];
  if (val2 != 2963090659u)
    __builtin_abort ();

  for (int i = 0; i < vl; i++)
    {
      uint8_t val = ((uint8_t *)&v3)[i];
      if (val != i << 2)
        __builtin_abort ();
    }
}
