/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d -mexplicit-relocs=auto -fno-section-anchors" } */

#include "explicit-relocs-auto-single-load-store.c"

/* { dg-final { scan-assembler-not "la.local" } } */
