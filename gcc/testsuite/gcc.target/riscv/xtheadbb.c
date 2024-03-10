/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadbb" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadbb" { target { rv64 } } } */

#ifndef __riscv_xtheadbb
#error "Feature macro not defined"
#endif

int
foo (int a)
{
  return a;
}

