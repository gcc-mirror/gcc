/* Test gcov block mode.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int one = 1; /* subvert constant folder. */
int zero = 0;

int foo (int ix)
{
  return ix & 1 ? one : zero; /* count(10) */
}

int main ()
{
  unsigned ix, jx = 0;
  
  for (ix = 10; ix--;) jx += foo (ix); /* count(11) */

  return jx != 5;
}

/* { dg-final { run-gcov { -a gcov-11.c } } } */
