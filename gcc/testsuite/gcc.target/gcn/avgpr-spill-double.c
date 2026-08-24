/* { dg-do compile } */
/* { dg-additional-options "-march=gfx908 -O1" } */
/* { dg-final { scan-assembler "accvgpr" } } */

typedef double v32df __attribute__ ((__vector_size__ (sizeof(double)*32)));
#define TYPE v32df

#include "avgpr-spill-int.c"
