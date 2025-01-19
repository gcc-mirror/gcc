/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv64gc_zvknhb" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zvknhb" { target { rv32 } } } */

#ifndef __riscv_zvknhb
#error "Feature macro not defined"
#endif

int
foo (int a)
{
  return a;
}
