/* { dg-do compile } */
/* { dg-options "-O2 --save-temps" } */

int d2i (double a);

int
foo (char *fmt, ...)
{
  int d, e;
  double f, g;
  __builtin_va_list ap;

  __builtin_va_start (ap, fmt);
  d = __builtin_va_arg (ap, int);
  f = __builtin_va_arg (ap, double);
  g = __builtin_va_arg (ap, double);
  d += d2i (f);
  d += d2i (g);
  __builtin_va_end (ap);

  /* { dg-final { scan-assembler-not "x7" } } */
  /* { dg-final { scan-assembler-not "q7" } } */
  return d;
}

/* { dg-final { cleanup-saved-temps } } */
