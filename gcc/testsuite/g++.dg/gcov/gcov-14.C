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
