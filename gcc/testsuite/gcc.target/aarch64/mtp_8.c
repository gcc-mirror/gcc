/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */
/* { dg-options "-O -mtp=tpidr_el2" } */

#include "mtp.c"

/* { dg-final { scan-assembler-times {mrs\tx[0-9]+, tpidr_el2} 1 } } */
