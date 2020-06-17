/* { dg-additional-options "-O2" } */
/* TODO:is there a way to automatically run the tests on various
   optimizations levels, and with/without debuginfo, rather than
   hardcoding options?  Adapt from torture .exp, presumably.  */


#include <stdlib.h>
#include <string.h>

int test_1 (void)
{
  return 0;
}
