/* Test if functions marked __attribute__((used)), but with address never
   taken in C code, don't use alternate calling convention for local
   functions on IA-32.  */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2" } */

extern void abort (void);

static int foo (int, int, int, int) __asm ("foo");
static __attribute__((noinline, used)) int
foo (int i, int j, int k, int l)
{
  return i + j + k + l;
}

void
bar (void)
{
  if (foo (1, 2, 3, 4) != 10)
    abort ();
}

int (*fn) (int, int, int, int);

void
baz (void)
{
  __asm ("movl $foo, %k0" : "=r" (fn));
  if (fn (2, 3, 4, 5) != 14)
    abort ();
}

int
main (void)
{
  bar ();
  baz ();
  return 0;
}
