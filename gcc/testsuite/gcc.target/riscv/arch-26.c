/* { dg-do compile } */
/* { dg-options "-march=rv32eh -mabi=ilp32e" } */
int foo() {}
/* { dg-error "'-march=rv32eh': h extension requires i extension" "" { target *-*-* } 0 } */
