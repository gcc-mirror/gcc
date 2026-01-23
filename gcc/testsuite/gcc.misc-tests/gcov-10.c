/* Test gcov block mode.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run } */

int main ()
{
  unsigned ix, jx = 0;
  
  for (ix = 10; ix--;) if (ix & 1) jx++; /* count(11) */

  return jx != 5;
}

/* { dg-final { run-gcov { -a gcov-10.c } } } */
