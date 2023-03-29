/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadba" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadba" { target { rv64 } } } */

#ifndef __riscv_xtheadba
#error Feature macro not defined
#endif

int
foo (int a)
{
  return a;
}

