/* { dg-do compile } */
/* { dg-options "-march=rv64idc_zcmp -mabi=lp64d" } */
int
foo ()
{}

/* { dg-error "zcd conflicts with zcmp" "" { target *-*-* } 0 } */
