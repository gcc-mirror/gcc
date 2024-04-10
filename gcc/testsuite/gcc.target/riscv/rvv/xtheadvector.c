/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadvector" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadvector" { target { rv64 } } } */

#ifndef __riscv_xtheadvector
#error "Feature macro not defined"
#endif

int
foo (int a)
{
  return a;
}
