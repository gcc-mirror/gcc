/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int main(int argc, char **argv)
{
  if (argc == 0)
    {
      int *ptr;
label:  /* count(#####) */
	{
	}
    }
  if (argc == 1)
    {
      __builtin_printf("hello\n");
    }
  return 0;
}

/* { dg-final { run-gcov gcov-pr90574-1.c } } */
