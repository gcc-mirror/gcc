/* PR rtl-optimization/48381 */

struct S { int s; } t;

int baz (void);
void fn (int, unsigned, int, unsigned, char);

static char
foo (signed x, unsigned y)
{
  return x < 0 || y >= 32 ? 1 : x >> y;
}

long long
bar (long long x, long y)
{
  return y < 0 ? 1LL : x - y;
}

void
test (int x, unsigned y, unsigned z, char w)
{
  unsigned v[2];
  fn (w || baz (), y, t.s, y, foo (bar (z, w) <= v[0], x));
}
