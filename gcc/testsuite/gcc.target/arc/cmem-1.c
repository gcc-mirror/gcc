/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=nps400 -mcmem" } */

#define CMEM_SECTION_ATTR __attribute__ ((section (".cmem")));

#include "cmem-st.inc"

/* { dg-final { scan-assembler "xst\\s" } } */
/* { dg-final { scan-assembler "xstw\\s" } } */
/* { dg-final { scan-assembler "xstb\\s" } } */
