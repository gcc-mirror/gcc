/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-require-effective-target pie } */
/* { dg-options "-fpie -pie" } */

#include <stdio.h>

static int __attribute__((noinline))
implementation (void)
{
  printf ("'ere I am JH\n");
  return 0;
}

static __typeof__ (implementation) *resolver (void)
{
  return (void *)implementation;
}

extern int magic (void) __attribute__ ((ifunc ("resolver")));

__attribute__ ((weak))
int
call_magic (int (*ptr) (void))
{
  return ptr ();
}

int main ()
{
  return call_magic (magic);
}
