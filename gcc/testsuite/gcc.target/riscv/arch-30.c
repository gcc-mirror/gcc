/* { dg-do compile } */
/* { dg-options "-march=rv64id_zcd_zcmp -mabi=lp64d" } */
int foo()
{
}

/* { dg-error "zcd conflicts with zcmp" "" { target *-*-* } 0 } */
