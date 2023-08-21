/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadmac" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadmac" { target { rv64 } } } */

#ifndef __riscv_xtheadmac
#error "Feature macro not defined"
#endif

int
foo (int a)
{
  return a;
}

