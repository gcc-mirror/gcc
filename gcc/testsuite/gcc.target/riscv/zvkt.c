/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvkt" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zvkt" { target { rv32 } } } */

#ifndef __riscv_zvkt
#error "Feature macro not defined"
#endif

int
foo (int a)
{
  return a;
}
