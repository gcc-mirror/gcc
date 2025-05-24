/* { dg-do compile } */
/* { dg-options "-O2" } */

#include "cold-attribute-4.c"

/* { dg-final { scan-assembler "movl" } } */
