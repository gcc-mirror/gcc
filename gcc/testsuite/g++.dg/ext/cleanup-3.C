/* { dg-do run } */
/* { dg-options "" } */
/* Verify that the cleanup handler receives the proper contents
   of the variable.  */

extern "C" void exit(int);
extern "C" void abort(void);

static int expected;

static void
handler(int *p)
{
  if (*p != expected)
    abort ();
}

static void __attribute__((noinline))
bar(void)
{
}

static void doit(int x, int y)
{
  int r __attribute__((cleanup (handler)));
  if (x < y)
    {
      r = 0;
      return;
    }

  bar();
  r = x + y;
}

int main()
{
  expected = 0;
  doit (1, 2);

  expected = 3;
  doit (2, 1);

  return 0;
}
