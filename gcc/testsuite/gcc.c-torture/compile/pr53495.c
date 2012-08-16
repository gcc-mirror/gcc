/* PR rtl-optimization/53495 */

int a, b, c, d, e, g;
static char
fn1 (char p1, int p2)
{
  return p1 || p2 < 0 || p2 >= 1 || 1 >> p2 ? p1 : 0;
}

static long long fn2 (int *, int);
static int fn3 ();
void
fn4 ()
{
  fn3 ();
  fn2 (&a, d);
}

long long
fn2 (int *p1, int p2)
{
  int f = -1L;
  for (; c <= 1; c++)
    {
      *p1 = 0;
      *p1 = fn1 (c, p2 ^ f);
    }
  a = 0;
  e = p2;
  return 0;
}

int
fn3 ()
{
  b = 3;
  for (; b; b--)
    c++;
  g = 0 >= c;
  return 0;
}
