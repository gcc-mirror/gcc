/* { dg-options "-fprofile-generate -ftest-coverage " } */
/* { dg-do run { target native } } */

int value;

extern "C" { void __gcov_dump(void); }

int main(int argc, char **argv)
{
  value = 123;					/* count(1) */

  for (unsigned i = 0; i < 100; i++)
    value += argc;				/* count(100) */

  __gcov_dump();

  for (unsigned i = 0; i < 1000; i++)		/* count(#####) */
    value += argc;

  return 0;					/* count(#####) */
}

/* { dg-final { run-gcov gcov-dump-1.C } } */
