/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-options "" } */

#include <stdio.h>

static int implementation (void)
{
  printf ("'ere I am JH\n");
  return 0;
}

static __typeof__ (implementation)* resolver (void)
{
  return (void *)implementation;
}

extern int magic (void) __attribute__ ((ifunc ("resolver"),visibility ("hidden")));

int main ()
{
  return magic () != 0;
}
