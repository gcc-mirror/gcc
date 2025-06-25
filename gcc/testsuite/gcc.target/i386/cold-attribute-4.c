/* { dg-do compile } */
/* { dg-options "-Oz" } */
#include <string.h>

int
__attribute__ ((cold))
t(int c)
{
  return -1;
}

/* { dg-final { scan-assembler "orl" } } */
