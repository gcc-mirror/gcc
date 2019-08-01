/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32i -march=rv32im_s_sx_unexpectedstring -mabi=ilp32" } */
int foo()
{
}
/* { dg-error "unexpected ISA string at end:" "" { target { "riscv*-*-*" } } 0 } */
