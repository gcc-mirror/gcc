/* { dg-do run } */
/* { dg-options "-O2 -fstack-clash-protection -fno-optimize-sibling-calls --param stack-clash-protection-probe-interval=12 --param stack-clash-protection-guard-size=12" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

/* For further testing, this can be run under valgrind where it's crashed
   on aarch64 and ppc64le with -fstack-check=specific.  */


__attribute__((noinline, noclone)) void
foo (char *p)
{
  asm volatile ("" : : "r" (p) : "memory");
}

__attribute__((noinline, noclone)) void
bar (void)
{
  char buf[131072];
  foo (buf);
}

__attribute__((noinline, noclone)) void
baz (void)
{
  char buf[12000];
  foo (buf);
}

int
main ()
{
  bar ();
  baz ();
  return 0;
}

