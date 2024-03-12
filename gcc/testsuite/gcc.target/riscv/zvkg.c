/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvkg" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zvkg" { target { rv32 } } } */

#ifndef __riscv_zvkg
#error "Feature macro not defined"
#endif

int
foo (int a)
{
  return a;
}
