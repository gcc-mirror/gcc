/* { dg-do run } */
/* { dg-additional-options "-fno-strict-aliasing" } */

struct a {
  int b;
} c;

int d;

static struct a
g()
{
  int a[2], b, f = 0;
  for (; f < 2; f++)
    a[f] = 1;
  b = a[0];
  if (b)
    return c;
}
int main()
{
  c.b = 1;
  struct a e = g();
  c = g();
  if (!c.b || !e.b)
    __builtin_abort();
  return 0;
}
