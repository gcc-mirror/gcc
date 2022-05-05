/* { dg-do compile } */
/* { dg-options "-march=rv32gf2 -mabi=ilp32" } */
int foo()
{
}
/* { dg-error "extension 'f' appear more than one time" "" { target *-*-* } 0 } */
