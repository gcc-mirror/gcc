/* { dg-do compile } */
/* { dg-options "-mcpu=nps400 -mcmem" } */

#define CMEM_SECTION_ATTR __attribute__ ((section (".cmem")));

#include "cmem-ld.inc"

/* { dg-final { scan-assembler "xld " } } */
/* { dg-final { scan-assembler "xldw " } } */
/* { dg-final { scan-assembler "xldb " } } */
