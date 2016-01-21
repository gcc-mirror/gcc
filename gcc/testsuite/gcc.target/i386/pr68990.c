/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O3 -march=x86-64" } */
/* { dg-final { scan-assembler-not "cmpl\[ \t]+(\[%a-z]+), \\1" } } */

short a;
int b = 1, f;
char c, e = 1;
long long d;

static short
foo ()
{
  unsigned g, h = 0;
  int i = 0 || d * (b | e);
  char j = a << i, l = a;
  short k;
  int m = -b;
  if (m < b)
    {
      k = m = b;
      g = (k || l) / (b / e);
      if (b)
	__builtin_printf ("foo=%lld\n", (long long) a);
    }
  a = b = m;
  if (j || e)
    {
      h = g;
      i = m;
      g = j * k / (i - d);
      if (m)
	b = j && b;
      e = b * (h & d) || g;
    }
  b = i;
  char n = e || h | d;
  e = i < d & k / n;
  return f;
}

int
main ()
{
  if (foo ())
    if (c)
    lab:
      goto lab;
  return 0;
}
