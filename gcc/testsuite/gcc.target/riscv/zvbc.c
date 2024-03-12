/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvbc" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zvbc" { target { rv32 } } } */

#ifndef __riscv_zvbc
#error "Feature macro not defined"
#endif

int
foo (int a)
{
  return a;
}
