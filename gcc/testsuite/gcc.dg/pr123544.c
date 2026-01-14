/* PR rtl-optimization/123544 */
/* { dg-do run { target bitint } } */
/* { dg-options "-O1 -w" } */

typedef unsigned char V __attribute__((vector_size (4)));
unsigned long long c;

signed char
foo  ()
{
  return 0;
}

unsigned int
bar (V x, short int y, long long z)
{
  _BitInt (15) a = 828;
  a &= (unsigned) z < 5 ? 0 : (unsigned) x[1];
  if (y)
    return a;
  for (unsigned short d; d; )
    ;
}

int
baz (unsigned char x)
{
  unsigned long long a[] = { };
  unsigned long long b = bar ((V) { 1, 1, 1 }, 1, -1);
  if (x)
    {
      c += b;
      return 0;
    }
  a[bar ((V) {}, 0, foo  ())];
}

int
main ()
{
  baz (1);
  if (c)
    __builtin_abort ();
}
