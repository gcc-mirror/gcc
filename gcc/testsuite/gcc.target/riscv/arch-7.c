/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32i -march=rv32im_s -mabi=ilp32" } */
int foo()
{
}
/* { dg-error ".'-march=rv32im_s': name of supervisor extension must be more than 1 letter" "" { target *-*-* } 0 } */
