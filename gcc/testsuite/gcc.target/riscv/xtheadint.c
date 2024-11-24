/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc_xtheadint" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadint" { target { rv64 } } } */

#ifndef __riscv_xtheadint
#error "Feature macro not defined"
#endif

int
foo (int a)
{
  return a;
}

