/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -mexplicit-relocs=auto -mcmodel=extreme -fno-plt" } */
/* { dg-final { scan-assembler-not "la.tls.\[lg\]d" { target tls_native } } } */

#include "./explicit-relocs-auto-tls-ld-gd.c"
