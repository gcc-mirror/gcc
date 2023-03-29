/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadsync" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadsync" { target { rv64 } } } */

#ifndef __riscv_xtheadsync
#error Feature macro not defined
#endif

int
foo (int a)
{
  return a;
}

