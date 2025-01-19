/* { dg-do compile { target { ! riscv_abi_e } } } */
/* Explicitly use -march flags to that we don't get V.  */
/* { dg-options "-O2 -fdump-rtl-ext_dce -march=rv64gc -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-O2 -fdump-rtl-ext_dce -march=rv32gc" { target { rv32 } } } */
/* { dg-final { scan-rtl-dump {Successfully transformed} "ext_dce" } } */

void
matrix_add_const(int N, short *A, short val)
{
    for (int j = 0; j < N; j++) {
      A[j] += val;
    }
}
