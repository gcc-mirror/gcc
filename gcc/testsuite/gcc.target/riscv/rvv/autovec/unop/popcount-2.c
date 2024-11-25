/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options "riscv_v" } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fno-vect-cost-model -fdump-tree-slp-details" } */

int x[8];
int y[8];

void foo ()
{
  x[0] = __builtin_popcount (y[0]);
  x[1] = __builtin_popcount (y[1]);
  x[2] = __builtin_popcount (y[2]);
  x[3] = __builtin_popcount (y[3]);
  x[4] = __builtin_popcount (y[4]);
  x[5] = __builtin_popcount (y[5]);
  x[6] = __builtin_popcount (y[6]);
  x[7] = __builtin_popcount (y[7]);
}

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 1 "slp2" } } */
/* { dg-final { scan-assembler "vcpop.v" { target { riscv_zvbb } } } } */
