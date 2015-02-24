/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -mhotpatch=1000000,1000000" } */

#include <stdio.h>

void hp1(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch(1000000,1000000)))
void hp2(void)
{
  printf("hello, world!\n");
}

__attribute__ ((hotpatch(1000001,1000000)))
void hp3(void)
{ /* { dg-error " requested .hotpatch. attribute is not a comma separated pair" } */
  printf("hello, world!\n");
}

__attribute__ ((hotpatch(1000000,1000001)))
void hp4(void)
{ /* { dg-error " requested .hotpatch. attribute is not a comma separated pair" } */
  printf("hello, world!\n");
}
