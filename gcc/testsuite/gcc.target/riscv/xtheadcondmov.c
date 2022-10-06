/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadcondmov" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadcondmov" { target { rv64 } } } */

#ifndef __riscv_xtheadcondmov
#error Feature macro not defined
#endif

int
foo (int a)
{
  return a;
}

