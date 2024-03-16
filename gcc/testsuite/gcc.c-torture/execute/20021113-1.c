/* { dg-require-effective-target alloca } */

/* This program tests a data flow bug that would cause constant propagation
   to propagate constants through function calls.  */

void abort (void);
void exit (int);

void
foo (int *p)
{
  *p = 10;
}

int
main(void)
{
  int *ptr = __builtin_alloca (sizeof (int));
  *ptr = 5;
  foo (ptr);
  if (*ptr == 5)
    abort ();
  exit (0);
}
