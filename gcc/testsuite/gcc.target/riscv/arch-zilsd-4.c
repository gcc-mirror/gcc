/* { dg-do compile } */
/* { dg-options "-march=rv32if_zcf_zclsd -mabi=ilp32f" } */
/* { dg-error "zclsd conflicts with zcf" "" { target *-*-* } 0 } */

int x;
