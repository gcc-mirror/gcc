/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch --save-temps" } */

#include <stdio.h>

/* { dg-prune-output "always_inline function might not be inlinable" } */
__attribute__ ((hotpatch(1,2)))
__attribute__ ((always_inline))
static void hp2(void)
{
  printf("hello, world!\n");
}

/* { dg-prune-output "called from here" } */
void hp1(void)
{
  hp2();
}
