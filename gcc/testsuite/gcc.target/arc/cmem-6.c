/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=nps400 -mcmem" } */

#define CMEM_SECTION_ATTR __attribute__ ((section (".cmem_shared")));

#include "cmem-ld.inc"

/* { dg-final { scan-assembler "xld\\s" } } */
/* { dg-final { scan-assembler "xldw\\s" } } */
/* { dg-final { scan-assembler "xldb\\s" } } */
