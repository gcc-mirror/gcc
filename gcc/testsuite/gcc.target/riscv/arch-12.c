/* { dg-do compile } */
/* { dg-options "-march=rv64im1p2p3 -mabi=lp64" } */
int foo() {}
/* { dg-error "'-march=rv64im1p2p3': for 'm1p2p\\?', version number with more than 2 level is not supported" "" { target *-*-* } 0 } */
