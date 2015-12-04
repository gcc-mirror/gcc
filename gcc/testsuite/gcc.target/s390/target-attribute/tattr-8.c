/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-mno-backchain -march=z13 -mzarch -O0" } */

#include <stdio.h>

#pragma GCC target("backchain")
void p1(void)
{
  printf ((void *)0);
}
#pragma GCC reset_options

__attribute__ ((target("backchain")))
void a1(void)
{
  printf ((void *)0);
}

/* { dg-final { scan-assembler-times "\tlg?r\t%r14,%r15" 2 } } */
