/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

char *str="dddd";

int foo1 (char** ptr)
{
  *ptr=str;
}

int foo (char *ptr)
{
  char *tmp = 0;
  foo1 (&ptr);
  tmp = ptr;
  return tmp[-1];
}

int mpx_test (int argc, const char **argv)
{
  printf ("%d\n", foo ("ddddd"));
  return 0;
}
