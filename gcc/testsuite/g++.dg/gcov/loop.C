/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

unsigned
loop (unsigned n, int value)		  /* count(14.0k) */
{
  for (unsigned i = 0; i < n - 1; i++)
  {
    value += i;				  /* count(21.0M) */
  }

  return value;
}

int main(int argc, char **argv)
{
  unsigned sum = 0;
  for (unsigned i = 0; i < 7 * 1000; i++)
  {
    sum += loop (1000, sum);
    sum += loop (2000, sum);		  /* count(7.0k) */
  }

  return 0;				  /* count(1) */
}

/* { dg-final { run-gcov branches { -abH loop.C } } } */
