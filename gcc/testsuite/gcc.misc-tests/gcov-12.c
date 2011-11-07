/* Test gcov weak ellision.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-require-weak "" } */
/* { dg-do run { target native } } */

int __attribute__ ((weak)) weak ()
{
  return 0;  /* count(1) */
}

int main ()
{
  return weak (); /* count(1) */
}

/* { dg-final { run-gcov { -a gcov-12.c } } } */
