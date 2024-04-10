/* { dg-do compile } */
/* { dg-options "-O2 -fno-inline -march=loongarch64 -mabi=lp64d -O2 -mcmodel=extreme -fno-plt -mexplicit-relocs=none -mdirect-extern-access" } */

#include "cmodel-extreme-mi-thunk-1.C"

/* { dg-final { scan-assembler "la.local\t\[^\n\]*\\.LTHUNK0" } } */
