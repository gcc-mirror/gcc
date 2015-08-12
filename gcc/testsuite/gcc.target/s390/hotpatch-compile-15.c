/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch" } */

#include <stdio.h>

__attribute__ ((hotpatch(1,2)))
static void hp1(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch(1,2)))
static inline void hp2(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch(0,0)))
__attribute__ ((always_inline))
static inline void hp3(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch(1,2)))
__attribute__ ((always_inline))
static inline void hp4(void)
{
  printf("hello, world!\n");
}

void main(void)
{
  hp1();
  hp2();
  hp3();
  hp4();
}
