/* { dg-options "-g" } */

#include "guality.h"

void __attribute__ ((__noinline__))
g (void)
{
  asm volatile ("");
}

int
f (int a)
{
  g ();
  GUALCHKVAL (a);
  return a;
}

int
main (int argc, char *argv[])
{
  f (argc + 2);
  f (argc + 5);
}
