/* { dg-do run } */
/* { dg-options "-Ofast" } */

/* The pow exponent does not fit operand_entry::count and was
   truncated to zero by reassoc, dropping the pow call.  */

extern double pow (double, double);

__attribute__((noipa)) double
f (double x, double y)
{
  return x * x * x * y * x * x * x * pow (x, 4294967296.0);
}

int
main (void)
{
  if (f (-1.0, 1.0) != 1.0)
    __builtin_abort ();
  return 0;
}
