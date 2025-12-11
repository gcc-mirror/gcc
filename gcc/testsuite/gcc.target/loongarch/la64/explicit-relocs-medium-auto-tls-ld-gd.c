/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -mexplicit-relocs=auto -mcmodel=medium -fplt" } */
/* { dg-final { scan-assembler-not "la.global" { target tls_native } } } */

#include "./explicit-relocs-auto-tls-ld-gd.c"
