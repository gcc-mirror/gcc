/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target ia32 } */
int a, b, c, d, e[2], f, l, m, n, o;
long long g = 1, j;
static unsigned int h;
static int i, k;

void
fn1 (long long p)
{
  int q = p;
  f = 1 ^ e[f ^ (q & 1)];
}

static void
fn2 (long long p)
{
  f = 1 ^ e[(f ^ 1) & 1];
  fn1 (p >> 1 & 1);
  fn1 (p >> 32 & 1);
}

void
fn3 (int p)
{
  g |= j = p;
}

int
main ()
{
  e[0] = 1;
  char p = l;
  h = --g;
  i = o = c;
  m = d ? 1 / d : 0;
  fn3 (l || 0);
  b = a;
  n = j++;
  k--;
  fn2 (g);
  fn2 (h);
  fn2 (i);

  if (k + f)
    __builtin_abort ();

  return 0;
}

