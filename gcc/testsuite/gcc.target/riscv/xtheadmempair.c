/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadmempair" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadmempair" { target { rv64 } } } */

#ifndef __riscv_xtheadmempair
#error Feature macro not defined
#endif

int
foo (int a)
{
  return a;
}
