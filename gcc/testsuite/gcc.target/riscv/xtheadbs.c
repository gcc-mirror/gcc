/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadbs" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadbs" { target { rv64 } } } */

#ifndef __riscv_xtheadbs
#error Feature macro not defined
#endif

int
foo (int a)
{
  return a;
}

