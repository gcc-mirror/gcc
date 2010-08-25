/* PR rtl-optimization/44858 */

extern void abort (void);
int a = 3;
int b = 1;

__attribute__((noinline)) long long
foo (int x, int y)
{
  return x / y;
}

__attribute__((noinline)) int
bar (void)
{
  int c = 2;
  c &= foo (1, b) > b;
  b = (a != 0) | c;
  return c;
}

int
main (void)
{
  if (bar () != 0 || b != 1)
    abort ();
  return 0;
}
