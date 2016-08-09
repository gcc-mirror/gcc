/* { dg-options "-fprofile-generate -ftest-coverage -lgcov" } */
/* { dg-do run { target native } } */

int value;

extern "C"
{
  void __gcov_dump(void);
  void __gcov_reset(void);
}

int main(int argc, char **argv)
{
  value = 123;					/* count(1) */

  for (unsigned i = 0; i < 100; i++)
    value += argc;				/* count(100) */

  __gcov_dump();

  for (unsigned i = 0; i < 1000; i++)		/* count(#####) */
    value += argc;

  __gcov_reset ();

  for (unsigned i = 0; i < 10000; i++)		/* count(10001) */
    value += argc;

  return 0;					/* count(1) */
}

/* { dg-final { run-gcov gcov-dump-2.C } } */
