/* Test argument passing of complex values.  The MIPS64 compiler had a
   bug when they were split between registers and the stack.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */

volatile _Complex float f1 = 1.1f + 2.2if;
volatile _Complex float f2 = 3.3f + 4.4if;
volatile _Complex float f3 = 5.5f + 6.6if;
volatile _Complex float f4 = 7.7f + 8.8if;
volatile _Complex float f5 = 9.9f + 10.1if;
volatile _Complex double d1 = 1.1 + 2.2i;
volatile _Complex double d2 = 3.3 + 4.4i;
volatile _Complex double d3 = 5.5 + 6.6i;
volatile _Complex double d4 = 7.7 + 8.8i;
volatile _Complex double d5 = 9.9 + 10.1i;
volatile _Complex long double ld1 = 1.1L + 2.2iL;
volatile _Complex long double ld2 = 3.3L + 4.4iL;
volatile _Complex long double ld3 = 5.5L + 6.6iL;
volatile _Complex long double ld4 = 7.7L + 8.8iL;
volatile _Complex long double ld5 = 9.9L + 10.1iL;

extern void abort (void);
extern void exit (int);

__attribute__((noinline)) void
check_float (int a, _Complex float a1, _Complex float a2,
	     _Complex float a3, _Complex float a4, _Complex float a5)
{
  if (a1 != f1 || a2 != f2 || a3 != f3 || a4 != f4 || a5 != f5)
    abort ();
}

__attribute__((noinline)) void
check_double (int a, _Complex double a1, _Complex double a2,
	     _Complex double a3, _Complex double a4, _Complex double a5)
{
  if (a1 != d1 || a2 != d2 || a3 != d3 || a4 != d4 || a5 != d5)
    abort ();
}

__attribute__((noinline)) void
check_long_double (int a, _Complex long double a1, _Complex long double a2,
	     _Complex long double a3, _Complex long double a4,
		   _Complex long double a5)
{
  if (a1 != ld1 || a2 != ld2 || a3 != ld3 || a4 != ld4 || a5 != ld5)
    abort ();
}

int
main (void)
{
  check_float (0, f1, f2, f3, f4, f5);
  check_double (0, d1, d2, d3, d4, d5);
  check_long_double (0, ld1, ld2, ld3, ld4, ld5);
  exit (0);
}
