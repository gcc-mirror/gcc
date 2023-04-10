/* { dg-do compile } */
/* { dg-options "-march=rv64if_zfinx -mabi=lp64" } */
int foo() {}
/* { dg-error "'-march=rv64if_zfinx': z\\*inx conflicts with floating-point extensions" "" { target *-*-* } 0 } */
