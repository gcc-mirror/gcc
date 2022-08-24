/* { dg-do compile } */
/* { dg-options "-march=rv32g2 -mabi=ilp32" } */
int foo()
{
}
/* { dg-warning "version of 'g' will be omitted, please specify version for individual extension" "" { target *-*-* } 0 } */
