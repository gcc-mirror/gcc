/* Test Gcov basics.  */

/* { dg-prms-id 8294 } */
/* { dg-options "-fprofile-arcs -ftest-coverage -g" } */
/* { dg-do run { target native } } */

void noop ()
{
}

int main ()
{
  int i;

  for (i = 0; i < 10; i++)	/* count(11) */
    noop ();			/* count(10) */

  return 0;			/* count(1) */
}

int a_variable = 0;
