/* PR c/23439 */
/* Origin: Serge Belyshev <belyshev@depni.sinp.msu.ru> */
/* { dg-do compile } */
/* { dg-options "" } */

# 0 "for-1.c"
void foo() { for  /* { dg-error "at end of input" "" { target *-*-* } 0 } */
