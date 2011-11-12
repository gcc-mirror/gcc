/* Test gcov multiple paths to file.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

#if !RECURSIVE
#define RECURSIVE 1
#include "./gcov-15.c"
#undef RECURSIVE
#endif

static void __attribute__ ((noinline)) Recursive (void);


#if RECURSIVE
static void __attribute__ ((noinline))
Recursive ()
{
  return; /* count(1) */
}

#else
int main ()
{
  Recursive (); /* count(1) */
  return 0;
}
#endif

/* { dg-final { run-gcov { -a gcov-15.c } } } */
