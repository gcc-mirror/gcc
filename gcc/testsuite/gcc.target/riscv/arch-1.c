/* { dg-do compile } */
/* { dg-options "-march=rv32i -march=rv32I -mabi=ilp32" } */
int foo()
{
}
/* { dg-error ".'-march=rv32I': first ISA subset must be 'e', 'i' or 'g'" "" { target *-*-* } 0 } */
