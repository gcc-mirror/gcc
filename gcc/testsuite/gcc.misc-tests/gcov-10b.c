/* Test gcov block mode.  */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int main ()
{
  unsigned ix, jx = 0;
  
  ix = 10; goto test; loop: ; if (ix & 1) jx++; test: ; if (ix--) goto loop; /* count(11) */

  return jx != 5;
}

/* { dg-final { run-gcov { -a gcov-10b.c } } } */

