/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fstack-protector-all -O2 -fpic" } */

#include "stack-protector-5.c"

/* See the comment in stack-protector-5.c.  */
/* { dg-final { scan-assembler-times {\tstr\t} 1 } } */
