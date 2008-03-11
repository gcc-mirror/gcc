/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

int __attribute__ ((noinline))
test (unsigned int *a, int b)
{
  return b ? 1 : __builtin_parity (*a);
}

int __attribute__ ((noinline))
testl (unsigned long *a, int b)
{
  return b ? 1 : __builtin_parityl (*a);
}

int __attribute__ ((noinline))
testll (unsigned long long *a, int b)
{
  return b ? 1 : __builtin_parityll (*a);
}

int
main ()
{
  unsigned int a = 0;
  unsigned long al;
  unsigned long long all;

  a = 0x12345670;
  if (test (&a, 0))
    abort ();

  al = 0x12345670ul;
  if (testl (&al, 0))
    abort();

#if 1
  all = 0x12345678abcdef0ull;
  if (testll (&all, 0))
    abort ();
#endif
  return 0;
}
