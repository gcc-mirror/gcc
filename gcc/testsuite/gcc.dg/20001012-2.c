/* { dg-do run { target sparc*-*-* } } */
/* { dg-options "-O2 -fpic" } */

extern void abort (void);
extern void exit (int);

void foo (char *x, const char *y, ...)
{
}

double bar (const char *x, long y)
{
  return 0.0;
}

typedef __SIZE_TYPE__ size_t;
extern size_t strlen (const char *);

double baz (const char *x)
{
  if (x[0] != '\0')
    {
      char y[6 + strlen (x)];
      foo (y, "FOO", x);
      return bar (y, 0);
    }

  return (__extension__ ((union { unsigned __l __attribute__((__mode__(__SI__))); float __d; }) { __l: 0x3f800000UL }).__d);
}

main ()
{
  if (baz("") != 1.0)
    abort ();
  exit (0);
}
