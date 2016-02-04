/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-mbackchain -march=z13 -mzarch -O0" } */

#include <stdio.h>

#pragma GCC target("no-backchain")
void p0(void)
{
  printf ((void *)0);
}
#pragma GCC reset_options

__attribute__ ((target("no-backchain")))
void a0(void)
{
  printf ((void *)0);
}

/* { dg-final { scan-assembler-not "\tlg?r\t%r14,%r15" } } */
