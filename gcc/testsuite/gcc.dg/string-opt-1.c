/* Ensure mempcpy is "optimized" into memcpy followed by addition.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

char *buffer;
char *test;

#define SIZE 100

char *
__attribute__((noinline))
my_memcpy (char *d, char *s, unsigned l)
{
  return __builtin_memcpy (d, s, l);
}

char *
__attribute__((noinline))
my_mempcpy (char *d, char *s, unsigned l)
{
  return __builtin_mempcpy (d, s, l);
}

void
run_test (char *d, char *s, unsigned l)
{
  char *r = my_mempcpy (d, s, l);
  if (r != d + l)
    __builtin_abort ();

  r = my_memcpy (d, s, l);
  if (r != d)
    __builtin_abort ();
}

int
main (void)
{
  const char* const foo = "hello world";
  unsigned l = __builtin_strlen (foo) + 1;

  buffer = __builtin_malloc (SIZE);
  __builtin_memcpy (buffer, foo, l);
  test = __builtin_malloc (SIZE);

  run_test (test, buffer, l);

  return 0;
}

/* { dg-final { scan-assembler-not "\<mempcpy\>" } } */
/* { dg-final { scan-assembler "memcpy" } } */
