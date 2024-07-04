/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -O3 -fdump-rtl-vsetvl-details" } */

#include "riscv_vector.h"
void
foo (int *in, int *out)
{
  vint32mf2_t v = *(vint32mf2_t *) (in + 100);
  *(vint32mf2_t *) (out + 200) = v;
}

/* { dg-final { scan-rtl-dump "Eliminate vsetvl_pre" "vsetvl" { target { no-opts "-O0" } } } }  */
