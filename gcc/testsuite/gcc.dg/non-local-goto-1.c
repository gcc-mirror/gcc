/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target nonlocal_goto } */

extern void abort (void);

int global;

static int foo(void) __attribute__((noinline));

static int foo(void)
{
  global = 1;
}

static int bar(void)
{
  foo ();
}

int execute(int cmd)
{
  __label__ start;

  void raise(void)
  {
    goto start;
  }

  int last;

  bar ();

  last = 0;

start:

  if (last == 0)
    while (1)
      {
        last = 1;
        raise ();
      }

  if (last == 0)
    return 0;
  else
    return cmd;
}

int main(void)
{
  if (execute (1) == 0)
    abort ();

  return 0;
}
