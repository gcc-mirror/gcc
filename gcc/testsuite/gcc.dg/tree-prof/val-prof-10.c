/* { dg-options "-O2 -fdump-rtl-expand -mtune=core2" } */
/* { dg-skip-if "" { ! { i?86-*-* x86_64-*-* } } } */

long buffer1[128], buffer2[128];
char *x;

void foo(long *r)
{
  x = (char *)r;
  asm volatile("" ::: "memory");
}

void
__attribute__((noinline))
compute()
{
  volatile int n = 24;
  __builtin_memcpy (buffer1, buffer2, n);
  foo (&buffer1[0]);
}

int
main()
{
  for (unsigned i = 0; i < 10000; i++)
    compute ();

  return 0;
}

/* { dg-final-use-not-autofdo { scan-rtl-dump "Selected stringop expansion strategy: rep_byte" "expand" } } */
