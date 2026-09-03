/* { dg-do compile } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-march=gfx908 -O1" } */
/* { dg-final { scan-assembler "accvgpr" } } */
/* { dg-final { scan-assembler-not "v_readlane" } } */

#define TYPE long

#include "simt-avgpr-spill-int.c"
