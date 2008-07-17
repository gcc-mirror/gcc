/* { dg-options "-O2" } */
/* { dg-do run } */

#if defined __i386__
#define REG "edi"
#else
#define REG "r14"
#endif

register unsigned long *ds asm(REG);

extern void abort (void);

__attribute__ ((noinline)) void
test (void)
{
  *++ds = 31337;
}

int
main ()
{
  unsigned long stack[2];
  stack[0] = 0;
  stack[1] = 0;
  ds = stack;
  test ();
  if (ds != stack + 1 || *ds != 31337)
    abort ();
  return 0;
}
