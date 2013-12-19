/* Functional tests for the function hotpatching feature.  */

/* { dg-do run } */
/* { dg-options "-O3 -mzarch -mhotpatch --save-temps" } */

#include <stdio.h>

__attribute__ ((hotpatch))
inline void hp1(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch))
__attribute__ ((always_inline))
void hp2(void) /* { dg-warning "always_inline function might not be inlinable" } */
{
  printf("hello, world!\n");
} /* { dg-warning "function 'hp2' with the 'always_inline' attribute is not hotpatchable" } */

int main (void)
{
  return 0;
}

/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler-not "nopr\t%r7" } } */
/* { dg-final { scan-assembler-not "nop\t0" } } */
