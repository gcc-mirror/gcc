/* { dg-do run { target *-*-linux* } } */
/* { dg-options "-O2 -D_FORTIFY_SOURCE=2" } */

#include <stdio.h>

extern int main(int argc, char** argv)
  __attribute__ ((__target__ ("no-sse,no-mmx")));

int main(int argc, char** argv)
{
  printf ("hello!\n");
  return 0;
}
