/* PR tree-optimization/79327 - wrong code at -O2 and -fprintf-return-value
   { dg-do run }
   { dg-options "-O2 -Wall" }  */

volatile int a, b = -1;
char buf[64];

#define FMT "%+03d%02d"
const char *volatile fmt = FMT;

int main ()
{
  int c = a;
  int d = b;
  if (c >= -35791395 && c < 35791394 && d >= -1 && d < __INT_MAX__)
    {
      /* In the following the range of return values can be computed
	 by GCC. */
      int n1 = __builtin_sprintf (buf, FMT, c + 1, d + 1);
      if (n1 > 7)
	__builtin_abort ();

      /* Here GCC can't see the format string so the return value
	 must be computed by a libc call.  */
      int n2 = __builtin_sprintf (buf, fmt, c + 1, d + 1);

      if (n1 != n2)
	__builtin_abort ();
    }
  return 0;
}
