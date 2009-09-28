
extern void abort(void);

__attribute__ ((noinline))
int foo(int n)
{
  if (n < 0)
    n = ~n;

  return n;
}

int main(void)
{
  if (foo (-1) != 0)
    abort ();

  return 0;
}
