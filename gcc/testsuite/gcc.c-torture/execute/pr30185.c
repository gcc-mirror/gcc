/* PR target/30185 */

extern void abort (void);

typedef struct S { char a; long long b; } S;

S
foo (S x, S y)
{
  S z;
  z.b = x.b / y.b;
  return z;
}

int
main (void)
{
  S a, b;
  a.b = 32LL;
  b.b = 4LL;
  if (foo (a, b).b != 8LL)
    abort ();
  a.b = -8LL;
  b.b = -2LL;
  if (foo (a, b).b != 4LL)
    abort ();
  return 0;
}
