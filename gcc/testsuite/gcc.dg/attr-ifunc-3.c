/* { dg-do run }  */
/* { dg-require-ifunc "" } */
/* { dg-options "" } */

#include <stdio.h>

static int __attribute__((noinline))
     implementation (void *ptr)
{
  if (ptr)
    return ((int (*) (void *))ptr) (0);
  
  printf ("'ere I am JH\n");
  return 0;
}

static void *resolver (void)
{
  return (void *)implementation;
}

extern int magic (void *) __attribute__ ((ifunc ("resolver")));

int main ()
{
  return magic ((void *)magic);
}
