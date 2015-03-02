/* { dg-options "-fprofile-arcs -ftest-coverage -Ofast" } */
/* { dg-do run { target native } } */

#include <iostream>

void __attribute__ ((noinline))
  Out (std::ostream &out, double x)
{ out << x << std::endl; } /* count(1) */

int main ()
{
  Out (std::cout, 1.5); /* count(1) */
  return 0;
}

/* { dg-final { run-gcov gcov-14.C } } */
/* run-gcov cleanups up after itself, but doesn't find all created gcov files.
   We could try to grep for ^Created in the exec log, but since there's only one
   testcase where we create these extra gcov files, do a local cleanup for
   now.  */
/* { dg-final { remote_file target delete iostream.gcov ostream.gcov locale_facets.h.gcov } } */
