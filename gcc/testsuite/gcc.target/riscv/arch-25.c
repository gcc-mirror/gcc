/* { dg-do compile } */
/* { dg-options "-march=rv64i_zcf -mabi=lp64" } */
int foo() {}
/* { dg-error "'-march=rv64.*zcf': zcf extension supports in rv32 only" "" { target *-*-* } 0 } */
