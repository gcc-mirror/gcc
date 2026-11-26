/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zclsd -mabi=ilp32d" } */
/* { dg-error "zclsd conflicts with zcf" "" { target *-*-* } 0 } */

int x;
