/* { dg-do compile } */
/* { dg-options "-march=rv64id_zcd_zcmt -mabi=lp64d" } */
int foo()
{
}

/* { dg-error "zcd conflicts with zcmt" "" { target *-*-* } 0 } */
