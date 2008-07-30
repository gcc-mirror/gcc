/* { dg-do run } */

extern void abort (void);

int global;

static void foo(void) __attribute__((noinline));

static void foo(void)
{
  global = 1;
}

static void bar(void)
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

  int last = -1;

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
