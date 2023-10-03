/* { dg-do compile } */
/* { dg-additional-options "-march=gfx908 -O1" } */
/* { dg-skip-if "incompatible ISA" { *-*-* } { "-march=gfx90[06]" } } */
/* { dg-final { scan-assembler "accvgpr" } } */

#define TYPE double

#include "avgpr-spill-int.c"
