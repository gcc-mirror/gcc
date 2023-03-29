/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zawrs" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zawrs" { target { rv32 } } } */

#ifndef __riscv_zawrs
#error Feature macro not defined
#endif

int
foo (int a)
{
  return a;
}
