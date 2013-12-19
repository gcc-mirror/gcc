/* Functional tests for the function hotpatching feature.  */

/* { dg-do run } */
/* { dg-options "-O3 -mzarch -mhotpatch=0" } */

#include <stdio.h>

void hp1(void)
{
  printf("hello, world!\n");
}

inline void hp2(void)
{
  printf("hello, world!\n");
}

__attribute__ ((always_inline))
void hp3(void) /* { dg-warning "always_inline function might not be inlinable" } */
{
  printf("hello, world!\n");
} /* { dg-warning "function 'hp3' with the 'always_inline' attribute is not hotpatchable" } */

int main (void)
{
  return 0;
}
