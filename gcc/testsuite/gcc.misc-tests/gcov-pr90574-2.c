/* { dg-options "-fprofile-arcs -ftest-coverage" } */
/* { dg-do run { target native } } */

int main(int argc, char **argv)
{
  switch (argc)
    {
    case 0:
      foo: /* count(#####) */
    case 1:;
    }
  return 0;
}

/* { dg-final { run-gcov gcov-pr90574-2.c } } */
