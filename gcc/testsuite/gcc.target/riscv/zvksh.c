/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvksh" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zvksh" { target { rv32 } } } */

#ifndef __riscv_zvksh
#error "Feature macro not defined"
#endif

int
foo (int a)
{
  return a;
}
