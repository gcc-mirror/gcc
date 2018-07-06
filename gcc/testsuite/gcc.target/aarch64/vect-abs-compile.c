
/* { dg-do compile } */
/* { dg-options "-O3 -fno-vect-cost-model" } */

#define N 16

#include "vect-abs.x"

/* XFAIL due to PR tree-optimization/64946.  */
/* { dg-final { scan-assembler "abs\\tv\[0-9\]+\.16b" { xfail *-*-* } } } */
/* { dg-final { scan-assembler "abs\\tv\[0-9\]+\.8h" { xfail *-*-* } } } */
/* { dg-final { scan-assembler "abs\\tv\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "abs\\tv\[0-9\]+\.2d" } } */
