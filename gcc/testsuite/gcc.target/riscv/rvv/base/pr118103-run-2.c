/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3" } */

#include "riscv_vector.h"

#define N 4
uint16_t a[N];
uint16_t b[N];
uint16_t c[N];

void initialize () {
  uint16_t tmp_0[N] = { 0xfff, 3213, 238, 275, };
  uint16_t tmp_1[N] = { 0x2,  823,  39,   9, };

  for (int i = 0; i < N; ++i)
    {
      a[i] = tmp_0[i];
      b[i] = tmp_1[i];
    }

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
  compute ();

  if (c[0] != 2048)
    __builtin_abort ();

  return 0;
}
