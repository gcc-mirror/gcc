/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d -mcmodel=extreme -mexplicit-relocs=auto -mrelax" } */
/* { dg-final { scan-assembler-not "R_LARCH_RELAX" { target tls_native } } } */

#include "tls-ie-relax.c"
