/* PR rtl-optimization/47157 */

struct S { unsigned a; unsigned b; } c = { 1, 0 };
unsigned long int e;
void bar (int);
int baz (void);

static int
foo (int x, short y)
{
  return ((x ^ y) & ((x ^ (x ^ y) & ~__INT_MAX__) - y ^ y)) < 0 ? x : x - y;
}

void
test (void)
{
  bar (foo (baz () != (c.a | c.b), -1L));
  for (e = 0; e; e = 1)
    ;
}
