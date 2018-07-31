/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int main (void)
{
  const int t = 2; /* count(1) */
  struct s1 {	/* count(1) */
    int x;
    int g[t];
  };

  struct s2 {
    int x;
    int g[2];
  };

  __builtin_printf("Sucess!\n");
  return 0;
}

/* { dg-final { run-gcov gcov-pr85350.c } } */
