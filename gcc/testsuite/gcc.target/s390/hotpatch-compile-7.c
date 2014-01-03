/* Functional tests for the function hotpatching feature.  */

/* { dg-do run } */
/* { dg-options "-O3 -mzarch -mno-hotpatch" } */

#include <stdio.h>

__attribute__ ((hotpatch))
void hp1(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch))
inline void hp2(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch))
__attribute__ ((always_inline))
void hp3(void) /* { dg-warning "always_inline function might not be inlinable" } */
{
  printf("hello, world!\n");
} /* { dg-warning "function 'hp3' with the 'always_inline' attribute is not hotpatchable" } */

__attribute__ ((hotpatch(0)))
void hp4(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch(0)))
inline void hp5(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch(0)))
__attribute__ ((always_inline))
void hp6(void) /* { dg-warning "always_inline function might not be inlinable" } */
{
  printf("hello, world!\n");
} /* { dg-warning "function 'hp6' with the 'always_inline' attribute is not hotpatchable" } */

__attribute__ ((hotpatch(1)))
void hp7(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch(1)))
inline void hp8(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch(1)))
__attribute__ ((always_inline))
void hp9(void) /* { dg-warning "always_inline function might not be inlinable" } */
{
  printf("hello, world!\n");
} /* { dg-warning "function 'hp9' with the 'always_inline' attribute is not hotpatchable" } */

int main (void)
{
  return 0;
}
