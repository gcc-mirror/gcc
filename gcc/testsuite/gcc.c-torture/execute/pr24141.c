// reduced testcase, compile with -O2. Also, with --disable-checking
// gcc produces wrong code.

void abort (void);
int i;

void g (void)
{
  i = 1;
}

void f (int a, int b)
{
  int c = 0;
  if (a == 0)
    c = 1;
  if (c)
    return;
  if (c == 1)
    c = 0;
  if (b == 0)
    c = 1;
  if (c)
    g ();
}

int main (void)
{
  f (1, 0);
  if (i != 1)
    abort ();
  return 0;
}
