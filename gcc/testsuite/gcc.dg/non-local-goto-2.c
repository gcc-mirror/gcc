/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

int global;

static foo(void) __attribute__((noinline));

static foo(void)
{
  global = 1;
}

static bar(void)
{
  foo ();
  global = 0;
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
