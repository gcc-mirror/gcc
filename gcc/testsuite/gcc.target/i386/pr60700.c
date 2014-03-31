/* PR rtl-optimization/60700 */
/* { dg-do run { target ia32 } } */
/* { dg-options "-O3 -march=i686" } */

int
__attribute__((noinline))
foo (void)
{
  return 0;
}

void *g = (void *)1;

struct st {
  char data[36]; /* must be greater than 32.  */
};

int
__attribute__((noinline))
repro(struct st **out)
{
  int status = 0;

  *out = 0;

  status = foo();
  if (status != 0) {
    return status;
  }

  if (0 == g) {
    status = 999;
    return status;
  }

  *out = (struct st *)__builtin_malloc(sizeof(struct st));
  if (0 == *out) {
    status = 42;
    return status;
  }

  __builtin_memset(*out, 0, sizeof(struct st));

  return status;
}

int
main ()
{
  struct st *p;
  int ret = repro (&p);
  unsigned int i;

  for (i = 0; i < sizeof (p->data)/sizeof (p->data[0]); i++)
    if (p->data[i] != 0)
      __builtin_abort ();

  return ret;
}
