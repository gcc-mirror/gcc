/* Test gcov weak ellision.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-require-weak "" } */
/* { dg-do run { target native } } */
/* { dg-additional-sources "gcovpart-13b.c" } */

int __attribute__ ((weak)) weak ()
{
  return 1;  /* count(-) */
}

int main ()
{
  return weak (); /* count(1) */
}

/* { dg-final { run-gcov { -a gcov-13.c } } } */
/* { dg-final { run-gcov { -a gcovpart-13b.c } } } */
