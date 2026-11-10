/* { dg-do compile } */
/* { dg-additional-options "-march=gfx908 -O1" } */
/* { dg-final { scan-assembler "accvgpr" } } */

typedef short v32hi __attribute__ ((__vector_size__ (sizeof(short)*32)));
#define TYPE v32hi

#include "avgpr-spill-int.c"
