/* { dg-do run } */
/* { dg-options "" } */
/* Verify cleanup execution on non-trivial exit from a block.  */

extern void exit(int);
extern void abort(void);

static int counter;

static void
handler(int *p)
{
  counter += *p;
}

static void __attribute__((noinline))
bar(void)
{
}

static void doit(int n, int n2)
{
  int i;
  for (i = 0; i < n; ++i)
    {
      int dummy __attribute__((cleanup (handler))) = i;
      if (i == n2)
	break;
      bar();
    }
}

int main()
{
  doit (10, 6);
  if (counter != 0 + 1 + 2 + 3 + 4 + 5 + 6)
    abort ();
  return 0;
}
