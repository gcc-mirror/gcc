/* Test gcov block mode.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int main ()
{
  unsigned ix;
  
  for (ix = 10; ix--;); /* count(11) */

  return 0;
}

/* { dg-final { run-gcov { -a gcov-9.c } } } */
