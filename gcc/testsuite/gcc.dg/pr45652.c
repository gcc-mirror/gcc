/* { dg-do run { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -fselective-scheduling2" } */

struct S {
  double i[2];
};

void __attribute__ ((noinline)) checkcd (struct S x)
{
  if (x.i[0] != 7.0 || x.i[1] != 8.0)
    __builtin_abort ();
}

void __attribute__ ((noinline)) testvacd (int n, ...)
{
  int i;
  __builtin_va_list ap;
  __builtin_va_start (ap, n);
  for (i = 0; i < n; i++)
    {
      struct S t = __builtin_va_arg (ap, struct S);
      checkcd (t);
    }
  __builtin_va_end (ap);
}

void
testitcd (void)
{
  struct S x = { { 7.0, 8.0 } };
  testvacd (2, x, x);
}

int
main ()
{
  testitcd ();
  return 0;
}
