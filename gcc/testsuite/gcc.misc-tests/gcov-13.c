/* Test gcov weak ellision.  */

/* { dg-do run { target native } } */
/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-require-weak "" } */
/* { dg-additional-sources "gcovpart-13b.c" } */
/* { dg-skip-if "weak ellision not supported" { { hppa*-*-hpux* } && { ! lp64 } } } */

int __attribute__ ((weak)) weak ()
{
  return 1;  /* count(-) { xfail *-*-* } */
}

int main ()
{
  return weak (); /* count(1) */
}

/* { dg-final { run-gcov { -a gcov-13.c } { xfail *-*-* } } } */
/* { dg-final { run-gcov { -a gcovpart-13b.c } } } */
