/* PR target/95252 */
/* { dg-options "-O3 -funroll-loops -msave-restore" } */
/* { dg-do run } */

int a[6], b = 1, d, e;
long long c;
static int f = 1;

void
fn1 (int p1)
{
  b = (b >> 1) & (1 ^ a[(1 ^ p1) & 5]);
}

void
fn2 ()
{
  b = (b >> 1) & (1 ^ a[(b ^ 1) & 1]);
  fn1 (c >> 1 & 5);
  fn1 (c >> 2 & 5);
  fn1 (c >> 4 & 5);
  fn1 (c >> 8 & 5);
}

int
main ()
{
  int i, j;
  for (; d;)
    {
      for (; e;)
	fn2 ();
      f = 0;
    }
  for (i = 0; i < 8; i++)
    {
      if (f)
	i = 9;
      for (j = 0; j < 7; j++)
	fn2 ();
    }

  if (b != 0)
    __builtin_abort ();

  return 0;
}
