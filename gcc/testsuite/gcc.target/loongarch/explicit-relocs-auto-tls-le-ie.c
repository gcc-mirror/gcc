/* { dg-do compile } */
/* { dg-options "-O2 -mexplicit-relocs=auto" } */

#include "explicit-relocs-auto-tls-ld-gd.c"

/* { dg-final { scan-assembler-not "la.tls" { target tls_native } } } */
