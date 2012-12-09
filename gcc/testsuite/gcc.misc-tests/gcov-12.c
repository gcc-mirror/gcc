/* Test gcov weak ellision.  */

/* { dg-do run { target native } } */
/* { dg-require-weak "" } */
/* { dg-options "-fprofile-arcs -ftest-coverage" } */

int __attribute__ ((weak)) weak ()
{
  return 0;  /* count(1) */
}

int main ()
{
  return weak (); /* count(1) */
}

/* { dg-final { run-gcov { -a gcov-12.c } } } */
