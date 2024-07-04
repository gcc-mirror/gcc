/* { dg-additional-options "-fexceptions" } */
/* TODO:is there a way to automatically run the tests on various
   optimizations levels, and with/without debuginfo, rather than
   hardcoding options?  Adapt from torture .exp, presumably.  */

#include <stdio.h>
int
main ()
{
  FILE *f = fopen ("conftest.out", "w");
  if (f == NULL)
    return 1;
  return ferror (f) || fclose (f) != 0;

  ;
  return 0;
}

