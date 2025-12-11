/* { dg-do compile } */
/* { dg-options "-O2 -mcmodel=normal -mexplicit-relocs -mno-relax" } */
/* { dg-final { scan-assembler-not "R_LARCH_RELAX" { target tls_native } } } */

#include "tls-ie-relax.c"
