/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-mzarch" } */

#include <stdio.h>

__attribute__ ((hotpatch(1,2)))
__attribute__ ((always_inline))
static inline void hp2(void)
{
  printf("hello, world!\n");
}

void hp1(void)
{
  hp2();
}
