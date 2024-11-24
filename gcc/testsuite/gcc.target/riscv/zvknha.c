/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv64gc_zvknha" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zvknha" { target { rv32 } } } */

#ifndef __riscv_zvknha
#error "Feature macro not defined"
#endif

int
foo (int a)
{
  return a;
}
