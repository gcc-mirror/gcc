/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv64gc_zvksed" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zvksed" { target { rv32 } } } */

#ifndef __riscv_zvksed
#error "Feature macro not defined"
#endif

int
foo (int a)
{
  return a;
}
