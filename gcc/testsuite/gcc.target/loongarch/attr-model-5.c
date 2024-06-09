/* { dg-do compile } */
/* { dg-options "-mexplicit-relocs=none -mcmodel=extreme -O2 -fno-pic" } */
/* { dg-final { scan-assembler "la.local\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,x" } } */
/* { dg-final { scan-assembler "la.local\t\\\$r\[0-9\]+,y" } } */
/* { dg-final { scan-assembler "la.local\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,counter" } } */

#define ATTR_MODEL_TEST
#include "attr-model-test.c"
