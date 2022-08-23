/* PR rtl-optimization/104814 */

short a = 0;
static long b = 0;
int c = 7;
char d = 0;
short *e = &a;
long f = 0;

unsigned long
foo (unsigned long h, long j)
{
  return j == 0 ? h : h / j;
}

int
main ()
{
  long k = f;
  for (; c; --c)
    {
      for (int i = 0; i < 7; ++i)
	;
      long m = foo (f, --b);
      d = ((char) m | *e) <= 43165;
    }
  if (b != -7)
    __builtin_abort ();
  return 0;
}
