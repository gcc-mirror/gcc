/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-skip-if "" { *-*-* } { "-flto" "-O0" "-Og" "-Os" "-Oz" } } */
/* { dg-options "-march=rv64gc_xtheadmemidx" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xtheadmemidx" { target { rv32 } } } */

volatile long long a;
int b;
int c[1];

void d()
{
  c[a] = b;
}

/* { dg-final { scan-assembler "th.srw\t" } } */
