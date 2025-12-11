/* { dg-do compile } */
/* { dg-options "-mexplicit-relocs -mcmodel=normal -O2" } */
/* { dg-final { scan-assembler-times "%pc64_hi12" 2 } } */

#define ATTR_MODEL_TEST
#include "attr-model-test.c"
