/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d" } */

#include "riscv_vector.h"

#define N 4
uint16_t a[N];
uint16_t b[N];
uint16_t c[N];

void initialize ()
{
  uint16_t tmp_0[N] = { 0xfff, 3213, 238, 275, };

  for (int i = 0; i < N; ++i)
    a[i] = b[i] = tmp_0[i];

  for (int i = 0; i < N; ++i)
    c[i] = 0;
}

void compute ()
{
  size_t vl = __riscv_vsetvl_e16m1 (N);
  vuint16m1_t va = __riscv_vle16_v_u16m1 (a, vl);
  vuint16m1_t vb = __riscv_vle16_v_u16m1 (b, vl);
  vuint16m1_t vc = __riscv_vaaddu_vv_u16m1 (va, vb, __RISCV_VXRM_RDN, vl);

  __riscv_vse16_v_u16m1 (c, vc, vl);
}

int main ()
{
  initialize ();
  compute();

  return 0;
}

/* { dg-final { scan-assembler-times {csrwi\s+vxrm,\s*[01234]} 2 } } */
