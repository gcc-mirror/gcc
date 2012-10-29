
/* { dg-do compile } */
/* { dg-options "-O3" } */

#define N 16

#include "vect-abs.x"

/* { dg-final { scan-assembler "abs\\tv\[0-9\]+\.16b" } } */
/* { dg-final { scan-assembler "abs\\tv\[0-9\]+\.8h" } } */
/* { dg-final { scan-assembler "abs\\tv\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "abs\\tv\[0-9\]+\.2d" } } */
