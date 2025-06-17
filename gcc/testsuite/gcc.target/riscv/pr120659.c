/* { dg-do compile } */
/* { dg-options "-mcpu=sifive-x280 -mabi=lp64" } */

_Float16 f;
void foo() { f /= 3; }
