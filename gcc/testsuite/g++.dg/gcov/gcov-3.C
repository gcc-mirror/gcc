/* Check that gcov doesn't abort when a static object is defined
   within a header file.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

#include "gcov-3.h"

extern int foo();

int
main ()
{
  return foo();                                /* count(1) */
}

/* { dg-final { run-gcov gcov-3.C } } */
