/* { dg-do compile } */
/* { dg-options "-O2 --save-temps" } */

int
foo (char *fmt, ...)
{
  int d;
  __builtin_va_list ap;

  __builtin_va_start (ap, fmt);
  d = __builtin_va_arg (ap, int);
  __builtin_va_end (ap);

  /* { dg-final { scan-assembler-not "x7" } } */
  return d;
}

/* { dg-final { cleanup-saved-temps } } */
