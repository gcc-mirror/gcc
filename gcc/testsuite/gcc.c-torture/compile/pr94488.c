/* PR target/94488 */

typedef unsigned long V __attribute__((__vector_size__(16)));
typedef long W __attribute__((__vector_size__(16)));

void
foo (V *x, unsigned long y)
{
  *x = *x >> (unsigned int) y;
}

void
bar (V *x, unsigned long y)
{
  *x = *x << (unsigned int) y;
}

void
baz (W *x, unsigned long y)
{
  *x = *x >> (unsigned int) y;
}
