/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-fstack-protector-all -O2 -fpic" } */

#include "stack-protector-5.c"

/* See the comment in stack-protector-5.c.  */
/* { dg-final { scan-assembler-times {\tldr\t[^\n]*__stack_chk_guard} 2 } } */
