/* builtin_return_address(n) with n>0 has always been troublesome ...
   especially when the S/390 packed stack layout comes into play.  */

/* { dg-do run } */
/* { dg-options "-O3 -fno-optimize-sibling-calls -mbackchain -mpacked-stack -msoft-float" } */

void *addr1;

extern void abort (void);

void * __attribute__((noinline))
foo1 ()
{
  addr1 = __builtin_return_address (2);
}

void * __attribute__((noinline))
foo2 ()
{
  foo1 ();
}

void * __attribute__((noinline))
foo3 ()
{
  foo2 ();
}

void __attribute__((noinline))
bar ()
{
  void *addr2;

  foo3 ();
  asm volatile ("basr  %0,0\n\t" : "=d" (addr2));
  /* basr is two bytes in length.  */
  if (addr2 - addr1 != 2)
    abort ();
}

int
main ()
{
  bar();
  return 0;
}
