/* Check that branch percentages are calculated in variables
   that are large enough to hold the count. */

/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

#define LIMIT1 7000
#define LIMIT2 7000

int count;

void incr_count ()
{
  count++;
}

void doit (int i, int j)
{
  if (i > j)
    incr_count ();
}

int main ()
{
  int i, j;

  for (i = 0; i < LIMIT1; i++)
    for (j = 0; j < LIMIT2; j++)
      doit (i, j);

  return 0;
}

/* { dg-final { run-gcov branches { -b gcov-5b.c } } } */
